
-- | A very simple implementation of the Levenshtein algorithm.

module Data.Text.Levenshtein where

import           Algebra.Structure.Semiring
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

import qualified Data.Vector.Align.Global.Linear2 as Align



-- | Levenshtein distance. This function is marked @inline@. It should be bound
-- where it is to be used, there marked @NoInline@. This allows enabling, say,
-- @LLVM@, in the using module.
--
-- This function is markedly worse than, say, the one in @text-metrics@. We
-- internally hold the complete DP matrix to enable backtracing.
--
-- TODO return the backtrace as well.

levenshtein ∷ Text → Text → Int
{-# Inline levenshtein #-}
levenshtein xs ys = getMinPlus . fst $ Align.nwScoreForward (\x y → if x==y then 0 else 1) (const 1) (const 1) xv yv
  where !xv = VU.fromList $ T.unpack xs
        !yv = VU.fromList $ T.unpack ys

