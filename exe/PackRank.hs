{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Distribution.PackDeps.Lens
import Distribution.PackRank
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

main :: IO ()
main = do
  n <- loadNewest
  let idx = makeIndex n
      dep = depending idx n
      s = packRank dep defaultAlpha defaultEpsilon
  ss <- G.thaw . G.imap (flip (,)) $ prVector s
  I.sort ss
  tt <- G.freeze ss :: IO (U.Vector (Double,Int))
  G.forM_ tt $ \(r,i) ->
    print (r, reverseIdx idx G.! i)
