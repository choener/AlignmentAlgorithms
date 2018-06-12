
-- | Simple global sequence alignment between two 'ByteString's.

module Data.Vector.Align.Global.Linear2 where

import           Data.PrimitiveArray
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import           Control.Monad.ST
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import           Data.Typeable

import           ADP.Fusion.Point
import           DP.Seq.Align.Global.Linear2



sScore
  ∷ (Monad m)
  ⇒ z
  -- ^ Zero for the score ring
  → z
  -- ^ One for the score ring
  → (z → z → z)
  -- ^ addition operation with neutral zero
  → (z → z → z)
  -- ^ multiplication operation with neutral one
  → (a → b → z)
  -- ^ align a with b
  → (a     → z)
  -- ^ insert a, with no b
  → (    b → z)
  -- ^ insert b, with no a
  → SigGlobal m z z a b
sScore zero one (<+>) (<*>) fAlign fDelin fIndel = SigGlobal
  { align = \x (Z:.a :.b ) → x <*> fAlign a b
  , indel = \x (Z:.():.b ) → x <*> fIndel   b
  , delin = \x (Z:.a :.()) → x <*> fDelin a
  , done  = const one
  , h     = SM.foldl' (<+>) zero
  }
{-# Inline sScore #-}

-- | Produces the forward score, together with additional information.

nwScoreForward
  ∷ (Typeable z, VU.Unbox z, VG.Vector v a, VG.Vector v b)
  ⇒ z
  -- ^ Zero for the score ring
  → z
  -- ^ One for the score ring
  → (z → z → z)
  -- ^ addition operation with neutral zero
  → (z → z → z)
  -- ^ multiplication operation with neutral one
  → (a → b → z)
  -- ^ align a with b
  → (a     → z)
  -- ^ insert a, with no b
  → (    b → z)
  -- ^ insert b, with no a
  → v a
  -- ^ first input vector with input type @a@
  → v b
  -- ^ second input with input type @b@
  → ( z
    , Mutated (Z:.TwITbl 0 0 Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) z)
    )
nwScoreForward zero one (<+>) (<*>) fAlign fDelin fIndel i1 i2
  = {-# SCC "nwScoreForward" #-} runST $ do
    arr ← newWithPA (ZZ:..LtPointL n1:..LtPointL n2) zero
    ret ← fillTables
        $ gGlobal (sScore zero one (<+>) (<*>) fAlign fDelin fIndel)
                  (ITbl @0 @0 (Z:.EmptyOk:.EmptyOk) arr)
                  (chr i1)
                  (chr i2)
    let a = let (Z:.r) = mutatedTables ret in unId $ axiom r
    return (a, ret)
    where n1 = VG.length i1
          n2 = VG.length i2
{-# Inline nwScoreForward #-}

