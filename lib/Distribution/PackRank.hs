{-# LANGUAGE BangPatterns, DeriveGeneric, GeneralizedNewtypeDeriving,
    RecordWildCards, ScopedTypeVariables #-}

module Distribution.PackRank
    (
      Depending(..)
    , Depended(..)
    , Index(..)
    , makeIndex
    , depending
    , PackRank(..)
    , packRank
    , defaultAlpha
    , defaultEpsilon
    ) where

import Control.Lens hiding (Index)
import Data.Binary
import Data.Bits ((.&.))
import Data.Function (on)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Vector.Binary ()
import Distribution.PackDeps.Lens
import GHC.Generics (Generic)
import Prelude hiding (pi)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- | This matrix maps packages to the packages they depend on.  The
-- outer vector is indexed by a package's ID, and the inner contains
-- the ID of every package it depends on.
newtype Depending = Depending (V.Vector (U.Vector Int))
                  deriving (Eq, Read, Show, Binary)

-- | This matrix maps packages to the packages that depend on them.
-- The outer vector is indexed by a package's ID, and the inner contains
-- the ID of every package that depends on it.
newtype Depended = Depended (V.Vector (U.Vector Int))
                 deriving (Eq, Read, Show, Binary)

-- | Indices of \"silent\" package IDs (those that are depended on by
-- other packages, but have no dependencies themselves).
type Silent = U.Vector Int

-- | Map from package ID to the reciprocal of the number of packages
-- it depends on.
type DepFactors = U.Vector Double

data Index = Index {
    forwardIdx :: Map String Int
  , reverseIdx :: V.Vector String
  } deriving (Eq, Read, Show, Generic)

instance Binary Index

data PackRank = PackRank {
      prIter   :: {-# UNPACK #-} !Int
    , prVector :: !(U.Vector Double)
    } deriving (Eq, Read, Show, Generic)

instance Binary PackRank

makeIndex :: Newest -> Index
makeIndex = toIndex . ifoldl' go (0, Map.empty, [])
  where go name (i, m, im) _ = (i', m', name:im)
          where !i'  = i + 1
                !m'  = Map.insert name i m
        toIndex (_, m, im) = Index m (G.reverse . G.fromList $ im)

depending :: Index -> Newest -> Depending
depending Index{..} = Depending . V.fromList . IntMap.elems .
                      ifoldl' go IntMap.empty
  where
    go name im pi =
      let deps = pi ^.. dependsOn . to (flip Map.lookup forwardIdx)
      in IntMap.insert (forwardIdx Map.! name) (G.fromList (catMaybes deps)) im

dependsOn :: Fold PackInfo String
dependsOn = piDesc . folded . diDeps . folded . depName . packageName

transpose :: Depending -> (Depended, DepFactors, Silent)
transpose (Depending dep) = (Depended deps, factors, silent)
  where
    factors = G.convert . G.map (recip . fromIntegral . G.length) $ dep
    silent  = G.convert . G.map fst . G.filter (G.null . snd) . G.imap (,) $ dep
    deps = G.generate (G.length dep) $ \i ->
           maybe G.empty G.fromList $ Map.lookup i incoming
      where incoming = G.ifoldl' step Map.empty dep
            step m0 i = G.foldl' (\m j -> Map.insertWith (++) j [i] m) m0

packRanks :: Depended -> DepFactors -> Silent -> Double -> [PackRank]
packRanks (Depended depended) factors silent alpha =
    iterate iter $ PackRank 0 (G.replicate count (1/n))
  where
    iter (PackRank k old0) = PackRank (k+1) (G.convert . G.map step $ depended)
      where
        step addr = h + a + i
          where
            h | G.null addr = 0
              | otherwise   = alpha * G.backpermute old addr `dot`
                                      G.backpermute factors addr
        i = (1 - alpha) * G.sum old / n
        a | G.null silent = 0
          | otherwise     = alpha * G.sum (G.backpermute old silent) / n
        old | k .&. 15 == 15 = G.map (/G.sum old0) old0
            | otherwise      = old0
    count = G.length factors
    n = fromIntegral count

packRank :: Depending -> Double -> Double -> PackRank
packRank outgoing alpha epsilon =
    snd . head . filter ((< epsilon * n) . fst) .
    take 8 . every 10 . zipWith dist xs . tail $ xs
  where
    (dep@(Depended depended), factors, silent) = transpose outgoing
    dist a b = ((distance `on` prVector) b a, b)
    xs = packRanks dep factors silent alpha
    n  = fromIntegral (G.length depended)

dot :: U.Vector Double -> U.Vector Double -> Double
dot a b = G.sum (G.zipWith (*) a b)

distance :: U.Vector Double -> U.Vector Double -> Double
distance a b = sqrt (d `dot` d)
    where d = G.zipWith (-) a b

every :: Int -> [a] -> [a]
every n = go 0
  where
    go !i (x:xs) | i < n-1   = go (i+1) xs
                 | otherwise = x : go 0 xs
    go _ _                   = []

defaultAlpha :: Double
defaultAlpha = 0.85

defaultEpsilon :: Double
defaultEpsilon = 0.0001
