
-- | Full-blown affine-scoring tree alignment grammar with all cases.
--
-- We currently do not produce the outside algorithm, since the
-- @AffineSmall2@ variant seems more useful in practice. However, if you
-- want just send a mail.

module DP.Tree.Align.Global.Affine2 where

import ADP.Fusion.Core
import FormalLanguage.CFG



[formalLanguage|
Verbose

Grammar: Global
N: T -- tree
N: F -- forest
N: Z -- tree for gaps
N: Y -- tree for affine gaps
N: P -- parent gap mode
N: G -- sibling gap together with P
T: n
S: [F,F]
[F,F] -> iter    <<< [T,T] [F,F]
[F,F] -> iter    <<< [T,Z] [F,G]
[F,F] -> iter    <<< [Z,T] [G,F]
[F,F] -> done    <<< [e,e]
[P,F] -> pfalign <<< [T,T] [P,F]
[P,F] -> pfdelin <<< [T,Z] [P,G]
[P,F] -> pfindel <<< [Y,T] [P,F]
[P,F] -> done    <<< [e,e]
[F,P] -> done    <<< [e,e]
[F,P] -> fpalign <<< [T,T] [F,P]
[F,P] -> fpdelin <<< [T,Y] [F,P]
[F,P] -> fpindel <<< [Z,T] [G,P]
[G,F] -> gfalign <<< [T,T] [G,F]
[G,F] -> gfdelin <<< [T,Z] [P,G]
[G,F] -> gfindel <<< [Y,T] [G,F]
[G,F] -> done    <<< [e,e]
[F,G] -> done    <<< [e,e]
[F,G] -> fgalign <<< [T,T] [F,F]
[F,G] -> fgdelin <<< [T,Y] [F,G]
[F,G] -> fgindel <<< [Z,T] [G,P]
[G,P] -> gpalign <<< [T,T] [F,P]
[G,P] -> gpdelin <<< [T,Y] [F,P]
[G,P] -> gpindel <<< [Y,T] [G,P]
[P,G] -> pgalign <<< [T,T] [P,F]
[P,G] -> pgdelin <<< [T,Y] [P,G]
[P,G] -> pgindel <<< [Y,T] [P,F]
[T,T] -> align   <<< [n,n] [F,F]
[Z,T] -> indel   <<< [-,n] [P,F]
[T,Z] -> delin   <<< [n,-] [F,P]
[Y,T] -> afindel <<< [-,n] [P,F]
[T,Y] -> afdelin <<< [n,-] [F,P]
//

Emit: Global
|]

makeAlgebraProduct ''SigGlobal

