
-- | An affine-scoring grammar for the alignment of two trees.
--
-- Due to the way @ADPfusion@ and @GADP@ works this grammar will, in
-- principle, work with basically any input type. In practice, the given
-- rules make most (or only) sense for tree alignments.

module DP.Tree.Align.Global.AffineSmall2 where

import ADP.Fusion.Core
import FormalLanguage.CFG



[formalLanguage|
Verbose

Grammar: Global
N: T -- tree
N: F -- forest
N: Z -- tree for gaps
N: Q -- sibling gap mode
N: R -- parent gap mode
N: E
T: n
S: [F,F]
[F,F] -> iter    <<< [T,T] [F,F]
[F,F] -> fgap    <<< [T,Z] [Q,Q]
[F,F] -> fgap    <<< [Z,T] [Q,Q]
[Z,T] -> indel   <<< [-,n] [R,R]
[T,Z] -> delin   <<< [n,-] [R,R]
[T,T] -> align   <<< [n,n] [F,F]
[F,F] -> done    <<< [E,E]
[R,R] -> done    <<< [E,E]
[R,R] -> pgap <<< [T,T] [R,R]
[R,R] -> pgap <<< [T,Z] [R,R]
[R,R] -> pgap <<< [Z,T] [R,R]
[Q,Q] -> done    <<< [E,E]
[Q,Q] -> siter <<< [T,T] [F,F]
[Q,Q] -> sgap <<< [T,Z] [Q,Q]
[Q,Q] -> sgap <<< [Z,T] [Q,Q]
[E,E] -> finalDone <<< [e,e]
//
Outside: Labolg
Source: Global
//

Emit: Global
Emit: Labolg
|]

makeAlgebraProduct ''SigGlobal

resig :: Monad m => SigGlobal m a b c d -> SigLabolg m a b c d
resig (SigGlobal gdo git gsi gal gin gde gfg gpg gsg gfi gh) = SigLabolg gdo git gsi gal gin gde gfg gpg gsg gfi gh
{-# Inline resig #-}

