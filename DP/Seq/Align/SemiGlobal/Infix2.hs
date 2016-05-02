
-- | Affine grammar with zero-cost prefixes and suffixes.

module DP.Seq.Align.SemiGlobal.Infix2 where

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
[P,U] -> prePU <<< [P,U] [-,u]

-- consume prefix on lower tape

[U,P] -> done  <<< [e,e]
[U,P] -> preUP <<< [U,P] [b,-]

-- normal affine grammar (but with additional options to transition to
-- prefix

[M,M] -> done  <<< [e,e]
[M,M] -> align <<< [M,M] [b,u]
[M,M] -> align <<< [D,D] [b,u]
[M,M] -> align <<< [I,I] [b,u]
[M,M] -> toPUM <<< [P,U] [-,u]
[M,M] -> toUPM <<< [U,P] [b,-]

-- affine deletions.

[D,D] -> openU <<< [M,M] [-,u]
[D,D] -> contU <<< [D,D] [-,u]
[D,D] -> openU <<< [I,I] [-,u]
[D,D] -> toPUD <<< [P,U] [-,u]
[D,D] -> toUPD <<< [U,P] [b,-]

[I,I] -> openL <<< [M,M] [b,-]
[I,I] -> openL <<< [D,D] [b,-]
[I,I] -> contL <<< [I,I] [b,-]
[I,I] -> toPUI <<< [P,U] [-,u]
[I,I] -> toUPI <<< [U,P] [b,-]

-- consume suffix on upper tape

[S,U] -> frSUM <<< [M,M] [-,u]
[S,U] -> frSUD <<< [D,D] [-,u]
[S,U] -> frSUI <<< [I,I] [-,u]
[S,U] -> sufSU <<< [S,U] [-,u]

[U,S] -> frUSM <<< [M,M] [b,-]
[U,S] -> frUSD <<< [D,D] [b,-]
[U,S] -> frUSI <<< [I,I] [b,-]
[U,S] -> sufUS <<< [U,S] [b,-]

-- we can go directly to the affine part, or start in a suffix system. No
-- extra costs here, because we already pay in @frSUM@ etc

[S,S] -> start <<< [M,M]
[S,S] -> start <<< [D,D]
[S,S] -> start <<< [I,I]
[S,S] -> start <<< [S,U]
[S,S] -> start <<< [U,S]

//

Emit: Infix
|]

makeAlgebraProduct ''SigInfix

