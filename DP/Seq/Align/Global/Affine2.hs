
module DP.Seq.Align.Global.Affine2 where

import           Data.FMList (FMList)
import           Data.Sequence (Seq,empty,(|>))
import           Data.Vector.Fusion.Stream.Monadic (Stream,toList)
import qualified Data.FMList as F

import           ADP.Fusion.Core
import           Data.PrimitiveArray hiding (toList)
import           FormalLanguage



-- | Define signature and grammar

[formalLanguage|
Verbose
Grammar: Gotoh
N: S
N: M
N: D
N: I
T: b
T: u
S: [S,S]
[S,S] -> start <<< [M,M]
[S,S] -> start <<< [D,D]
[S,S] -> start <<< [I,I]

[M,M] -> done  <<< [e,e]
[M,M] -> align <<< [M,M] [b,u]
[M,M] -> align <<< [D,D] [b,u]
[M,M] -> align <<< [I,I] [b,u]

[D,D] -> openU <<< [M,M] [-,u]
[D,D] -> contU <<< [D,D] [-,u]
[D,D] -> openU <<< [I,I] [-,u]

[I,I] -> openL <<< [M,M] [b,-]
[I,I] -> openL <<< [D,D] [b,-]
[I,I] -> contU <<< [I,I] [b,-]
//

Emit: Gotoh
|]

makeAlgebraProduct ''SigGotoh

{-
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

-- | Backtracking with more options

backtrackFun :: Monad m => (l -> u -> r) -> (l -> u -> r) -> u -> l -> SigGlobal m (FMList r) [FMList r] l u
backtrackFun f g ud ld = SigGlobal
  { done  = \ _ -> F.empty
  , align = \ x (Z:.l:.u) -> x `F.snoc` f l  u
  , indel = \ x (Z:._:.u) -> x `F.snoc` g ld u
  , delin = \ x (Z:.l:._) -> x `F.snoc` g l  ud
  , h     = toList
  }
{-# Inline backtrackFun #-}

-- | Turn a single @FMList@ backtracking result into the corresponding
-- list.

runBacktrack :: FMList r -> [r]
runBacktrack = F.toList
{-# Inline runBacktrack #-}
-}

