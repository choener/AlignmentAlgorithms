
module DP.HMM.States2 where

import           ADP.Fusion.Core
import           Data.PrimitiveArray hiding (toList)
import           FormalLanguage



[formalLanguage|
Verbose
Grammar: HMM
N: P  -- plus case
N: M  -- minus case
T: x
S: M  -- we should start and stop here
P -> pstay <<< P x
P -> mtop  <<< M x
M -> mstay <<< M x
M -> ptom  <<< P x
M -> done  <<< e
//
Emit: HMM
|]

makeAlgebraProduct ''SigHMM

