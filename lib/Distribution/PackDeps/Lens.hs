{-# LANGUAGE TemplateHaskell #-}

module Distribution.PackDeps.Lens
    (
      P.PackInfo
    , piVersion
    , piDesc
    , piEpoch
    , P.DescInfo
    , diHaystack
    , diDeps
    , diPackage
    , diSynopsis
    , P.Newest
    , P.loadNewest
    , Dependency
    , depName
    , depRange
    , PackageName(..)
    , packageName
    ) where

import Control.Lens hiding (makeLenses)
import Distribution.PackDeps.Lens.Machinery (makeLenses)
import Distribution.Package hiding (packageName)
import Distribution.Version (VersionRange)
import qualified Distribution.PackDeps as P

makeLenses ''P.PackInfo
makeLenses ''P.DescInfo

depName :: Lens' Dependency PackageName
depName = lens (\(Dependency n _) -> n)
               (\(Dependency _ r) n -> (Dependency n r))

depRange :: Lens' Dependency VersionRange
depRange = lens (\(Dependency _ r) -> r)
                (\(Dependency n _) r -> (Dependency n r))

packageName :: Iso' PackageName String
packageName = iso (\(PackageName n) -> n) PackageName
