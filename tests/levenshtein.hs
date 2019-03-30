
module Main where

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Metrics as TM
import           Criterion
import           Criterion.Main

import qualified Data.Text.Levenshtein as TL


dtl, tml ∷ Text → Text → Int

{-# NoInline dtl #-}
dtl = TL.levenshtein

{-# NoInline tml #-}
tml = TM.levenshtein


main ∷ IO ()
main = do
  let gen = concat $ repeat [ 'a' .. 'z' ]
  let !i_10   = T.pack $ take   10 gen
  let !i_100  = T.pack $ take  100 gen
  let !i_1000 = T.pack $ take 1000 gen
  print $ dtl i_10 i_10
  print $ tml i_10 i_10
  defaultMain
    [ bgroup "   10   10" [ bench "tml" $ whnf (uncurry tml) (i_10,i_10)
                          , bench "dtl" $ whnf (uncurry dtl) (i_10,i_10)
                          ]
    , bgroup "  100  100" [ bench "tml" $ whnf (uncurry tml) (i_100,i_100)
                          , bench "dtl" $ whnf (uncurry dtl) (i_100,i_100)
                          ]
    , bgroup "1000 1000" [ bench "tml" $ whnf (uncurry tml) (i_1000,i_1000)
                         , bench "dtl" $ whnf (uncurry dtl) (i_1000,i_1000)
                         ]
    ]

