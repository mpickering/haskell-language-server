module Development.IDE.Plugin.CodeAction.Rules
  ( rulePackageExports
  )
where

import           Data.Traversable               ( forM )
import           Development.IDE.Core.Rules
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.CodeAction.RuleTypes
import           Development.IDE.Types.Exports
import           Development.Shake

rulePackageExports :: Rules ()
rulePackageExports = defineNoFile $ \(PackageExports session) -> do
  return $ mempty
