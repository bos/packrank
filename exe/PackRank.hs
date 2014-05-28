{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>))
import Control.Exception (try)
import Data.Binary (decodeFileOrFail, encodeFile)
import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes)
import Distribution.PackDeps.Lens
import Distribution.PackRank
import System.Directory (getAppUserDataDirectory, getModificationTime)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as Map
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

main :: IO ()
main = do
  (idx,s) <- getRanks
  dump idx (prVector s)

dump :: Index -> U.Vector Double -> IO ()
dump idx s = do
  ss <- G.thaw . G.imap (flip (,)) $ s
  I.sort ss
  tt <- G.freeze ss :: IO (U.Vector (Double,Int))
  let k = 10000 / fst (G.last tt)
  G.forM_ tt $ \(r,i) ->
    putStrLn . unwords $ [reverseIdx idx G.! i, show (round (r*k)::Int)]

getRanks :: IO (Index, PackRank)
getRanks = maybe computeRanks return =<< loadRanks

loadRanks :: IO (Maybe (Index, PackRank))
loadRanks = do
  it <- minimum <$> (mapM getModificationTime =<< findIndices)
  rp <- rankPath
  rt <- either (\(_::IOError) -> it) id <$> try (getModificationTime rp)
  if rt <= it
    then return Nothing
    else either (const Nothing) Just <$> decodeFileOrFail rp

computeRanks :: IO (Index, PackRank)
computeRanks = do
  status "loading"
  n <- loadNewest
  status $ show (Map.size n) ++ " packages"
  let idx = makeIndex n
      dep = depending idx n
  status "ranking"
  let !s = packRank dep defaultAlpha defaultEpsilon
  rp <- rankPath
  encodeFile rp (idx, s)
  return (idx, s)

status :: String -> IO ()
status = hPutStrLn stderr

rankPath :: IO FilePath
rankPath = do
  c <- getAppUserDataDirectory "cabal"
  return (c </> "packrank")

findIndices :: IO [FilePath]
findIndices = do
  c <- getAppUserDataDirectory "cabal"
  cfg <- readFile (c </> "config")
  let repos        = reposFromConfig cfg
      repoCache    = case lookupInConfig "remote-repo-cache" cfg of
          []        -> c </> "packages"  -- Default
          (rrc : _) -> rrc               -- User-specified
      tarName repo = repoCache </> repo </> "00-index.tar"
  return (map tarName repos)

reposFromConfig :: String -> [String]
reposFromConfig = map (takeWhile (/= ':')) . lookupInConfig "remote-repo"

lookupInConfig :: String -> String -> [String]
lookupInConfig key = map trim . catMaybes . map (stripPrefix prefix) . lines
  where
    prefix = key ++ ":"

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
