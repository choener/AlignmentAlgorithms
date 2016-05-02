
-- | Simple, linear tree editing grammar.

module DP.Tree.Edit.Global.Linear2 where

import ADP.Fusion
import FormalLanguage.CFG



[formalLanguage|
Verbose

Grammar: Global
N: T
N: F
T: x
S: [F,F]
[F,F] -> iter   <<< [F,F] [T,T]
[F,F] -> indel  <<< [F,F] [-,x]
[F,F] -> delin  <<< [F,F] [x,-]
[T,T] -> align  <<< [F,F] [x,x]
[F,F] -> done   <<< [e,e]
//

Emit: Global
|]


makeAlgebraProduct ''SigGlobal

