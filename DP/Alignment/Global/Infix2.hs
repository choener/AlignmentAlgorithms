
-- | Affine grammar with zero-cost prefixes and suffixes.

module DP.Alignment.Global.Infix2 where

import           Data.FMList (FMList)
import           Data.Sequence (Seq,empty,(|>))
import           Data.Vector.Fusion.Stream.Monadic (Stream,toList)
import qualified Data.FMList as F

import           ADP.Fusion
import           Data.PrimitiveArray hiding (toList)
import           FormalLanguage
import           FormalLanguage.GrammarProduct



-- | Define signature and grammar

[grammarProduct|
Verbose

Grammar: Infix
N: S
N: P
N: U
N: M
N: D
N: I
N: S
T: b
T: u
S: S

-- consume prefix on upper tape

[P,U] -> done  <<< [e,e]
[P,U] -> prePU <<< [-,u] [P,U]

-- consume prefix on lower tape

[U,P] -> done  <<< [e,e]
[U,P] -> preUP <<< [b,-] [U,P]

-- normal affine grammar (but with additional options to transition to
-- prefix

[M,M] -> done  <<< [e,e]
[M,M] -> align <<< [M,M] [b,u]
[M,M] -> align <<< [D,D] [b,u]
[M,M] -> align <<< [I,I] [b,u]
[M,M] -> toPUM <<< [P,U]
[M,M] -> toUPM <<< [U,P]

-- affine deletions.

[D,D] -> openU <<< [M,M] [-,u]
[D,D] -> contU <<< [D,D] [-,u]
[D,D] -> openU <<< [I,I] [-,u]
[D,D] -> toPUD <<< [P,U]
[D,D] -> toUPD <<< [U,P]

[I,I] -> openL <<< [M,M] [b,-]
[I,I] -> openL <<< [D,D] [b,-]
[I,I] -> contU <<< [I,I] [b,-]
[I,I] -> toPUI <<< [P,U]
[I,I] -> toUPI <<< [U,P]

-- consume suffix on upper tape

[S,U] -> frSUM <<< [M,M]
[S,U] -> frSUD <<< [D,D]
[S,U] -> frSUI <<< [I,I]
[S,U] -> sufSU <<< [S,U] [-,u]

[U,S] -> frUSM <<< [M,M]
[U,S] -> frUSD <<< [D,D]
[U,S] -> frUSI <<< [I,I]

-- we can go directly to the affine part, or start in a suffix system. No
-- extra costs here, because we already pay in @frSUM@ etc

[S,S] -> start <<< [M,M]
[S,S] -> start <<< [D,D]
[S,S] -> start <<< [I,I]
[S,S] -> start <<< [S,U]
[S,S] -> start <<< [U,S]

//

|]

--makeAlgebraProduct ''SigInfix

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

