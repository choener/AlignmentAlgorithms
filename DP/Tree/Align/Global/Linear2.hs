
-- | Simple, linear scoring for tree alignments.

module DP.Tree.Align.Global.Linear2 where

import ADP.Fusion
import FormalLanguage.CFG



[formalLanguage|
Verbose

Grammar: Global
N: T
N: F
N: M
T: n
S: [F,F]
[F,F] -> iter  <<< [T,T] [F,F]
[F,F] -> iter  <<< [M,M] [F,F]
[T,T] -> indel <<< [-,n] [F,F]
[T,T] -> delin <<< [n,-] [F,F]
[M,M] -> align <<< [n,n] [F,F]
[F,F] -> done  <<< [e,e]
//
Outside: Labolg
Source: Global
//

Emit: Global
Emit: Labolg
|]

makeAlgebraProduct ''SigGlobal

resig :: Monad m => SigGlobal m a b c d -> SigLabolg m a b c d
resig (SigGlobal gdo git gal gin gde gh) = SigLabolg gdo git gal gin gde gh
{-# Inline resig #-}

