module Distribution.PackDeps.Lens.Machinery (makeLenses) where

import Control.Lens ((&), (.~))
import Control.Lens.TH (DefName(..), lensField, lensRules, makeLensesWith)
import Language.Haskell.TH.Syntax (Dec, Name, Q, mkName, nameBase)

fieldName :: (String -> String) -> [Name] -> Name -> [DefName]
fieldName f _ name = [TopName . mkName . f . nameBase $ name]

makeLenses :: Name -> Q [Dec]
makeLenses = makeLensesWith (lensRules & lensField .~ fieldName id)
