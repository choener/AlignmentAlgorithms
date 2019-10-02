
module DP.HMM.States2 where

import           ADP.Fusion.Core
import           Data.PrimitiveArray hiding (toList)
import           FormalLanguage



[formalLanguage|
Verbose
Grammar: HMM
N: P
N: M
T: x
S: M
P -> pstay <<< P x
P -> mtop  <<< M x
M -> mstay <<< M x
M -> ptom  <<< P x
M -> done  <<< e
//
Emit: HMM
|]

makeAlgebraProduct ''SigHMM

