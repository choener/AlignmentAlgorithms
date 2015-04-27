
-- | Very simple pairwise global alignment. The terminal tapes may contain
-- the atomic types @u@ and @l@ which means that one may align sequences of
-- different types.
--
-- In case you want to align nucleotides to amino acids, this version
-- should only be used if the nucleotides are already in triplet form and
-- have no frameshift within the sequence. Alternatively, specify a derived
-- grammar of higher complexity.

module DP.Alignment.Global.Tapes2 where

import           Data.Sequence (Seq,empty,(|>))
import           Data.Vector.Fusion.Stream.Monadic (Stream)
import qualified Data.FMList as F
import           Data.FMList (FMList)

import ADP.Fusion
import Data.PrimitiveArray
import FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Grammar: Global
N: X
T: u
T: l
S: [X,X]
[X,X] -> done  <<< [e,e]
[X,X] -> align <<< [X,X] [u,l]
[X,X] -> indel <<< [X,X] [-,l]
[X,X] -> delin <<< [X,X] [u,-]
//

Emit: Global
|]

makeAlgebraProductH ['h] ''SigGlobal

-- | Generic backtracking scheme.
--
-- NOTE @Seq@ is strict ...

pretty :: Monad m => u -> l -> SigGlobal m (Seq (u,l)) (Stream m (Seq (u,l))) u l
pretty ud ld = SigGlobal
  { done  = \ _ -> empty
  , align = \ x (Z:.l:.u) -> x |> (u ,l )
  , indel = \ x (Z:._:.u) -> x |> (u ,ld)
  , delin = \ x (Z:.l:._) -> x |> (ud,l )
  , h     = return . id
  }
{-# Inline pretty #-}

-- | Generic backtracking scheme via @FMList@s.

prettyF :: Monad m => u -> l -> SigGlobal m (FMList (u,l)) (Stream m (FMList (u,l))) u l
prettyF ud ld = SigGlobal
  { done  = \ _ -> F.empty
  , align = \ x (Z:.l:.u) -> x `F.snoc` (u ,l )
  , indel = \ x (Z:._:.u) -> x `F.snoc` (u ,ld)
  , delin = \ x (Z:.l:._) -> x `F.snoc` (ud,l )
  , h     = return . id
  }
{-# Inline prettyF #-}

-- | Turn a single @FMList@ backtracking result into the corresponding
-- list.

runPrettyF :: FMList (u,l) -> [(u,l)]
runPrettyF = F.toList
{-# Inline runPrettyF #-}

