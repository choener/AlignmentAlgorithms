
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
import           Data.Vector.Fusion.Stream.Monadic (Stream,toList)
import qualified Data.FMList as F
import           Data.FMList (FMList)

import ADP.Fusion
import Data.PrimitiveArray hiding (toList)
import FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Grammar: Global
N: X
T: l
T: u
S: [X,X]
[X,X] -> done  <<< [e,e]
[X,X] -> align <<< [X,X] [l,u]
[X,X] -> indel <<< [X,X] [-,u]
[X,X] -> delin <<< [X,X] [l,-]
//

Emit: Global
|]

makeAlgebraProduct ''SigGlobal

-- | Generic backtracking scheme via @FMList@s.

backtrack :: Monad m => u -> l -> SigGlobal m (FMList (l,u)) [FMList (l,u)] l u
backtrack ud ld = SigGlobal
  { done  = \ _ -> F.empty
  , align = \ x (Z:.l:.u) -> x `F.snoc` (l ,u )
  , indel = \ x (Z:._:.u) -> x `F.snoc` (ld,u )
  , delin = \ x (Z:.l:._) -> x `F.snoc` (l ,ud)
  , h     = toList
  }
{-# Inline backtrack #-}

-- | Turn a single @FMList@ backtracking result into the corresponding
-- list.

runBacktrack :: FMList (u,l) -> [(u,l)]
runBacktrack = F.toList
{-# Inline runBacktrack #-}

