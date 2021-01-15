-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | General utility functions, mostly focused around GHC operations.
module Development.IDE.GHC.Util(
    -- * HcsEnv and environment
    HscEnvEq,
    hscEnv, newHscEnvEq,
    hscEnvWithImportPaths,
    envImportPaths,
    modifyDynFlags,
    evalGhcEnv,
    deps,
    -- * GHC wrappers
    prettyPrint,
    printRdrName,
    printName,
    ParseResult(..), runParser,
    lookupPackageConfig,
    textToStringBuffer,
    bytestringToStringBuffer,
    stringBufferToByteString,
    moduleImportPath,
    cgGutsToCoreModule,
    fingerprintToBS,
    fingerprintFromStringBuffer,
    -- * General utilities
    readFileUtf8,
    hDuplicateTo',
    setHieDir,
    dontWriteHieFiles,
    disableWarningsAsErrors,
    newHscEnvEqPreserveImportPaths,
    newHscEnvEqWithImportPaths) where

import Control.Concurrent
import Data.List.Extra
import Data.ByteString.Internal (ByteString(..))
import Data.Maybe
import Data.Typeable
import qualified Data.ByteString.Internal as BS
import GHC.Utils.Fingerprint
import GHC.Driver.Monad
import Control.Exception
import Data.IORef
import GHC.SysTools.FileCleanup
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import GHC.IO.BufferedIO (BufferedIO)
import GHC.IO.Device as IODevice
import GHC.IO.Encoding
import GHC.IO.Exception
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import Data.Unique
import Development.Shake.Classes
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString          as BS
import GHC.Parser.Lexer
import GHC.Data.StringBuffer
import System.FilePath
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModDetails
import GHC.Runtime.Context
import GHC.Driver.Env
--import UnitInfo (UnitInfo)
import GHC.Utils.Outputable
import GHC.Unit.State (unitInfoMap, lookupUnit', preloadClosure)
import GHC.Types.SrcLoc (mkRealSrcLoc)
import GHC.Data.FastString (mkFastString)
import GHC.Unit.Module.Name (moduleNameSlashes)
import GHC.Unit
import GHC.Types.Name.Occurrence (parenSymOcc)
import GHC.Types.Name.Reader (nameRdrName, rdrNameOcc)
import GHC.Unit.Env
import GHC.Driver.Config

import Development.IDE.GHC.Compat as GHC
import Development.IDE.Types.Location
import System.Directory (canonicalizePath)


----------------------------------------------------------------------
-- GHC setup

-- | Used to modify dyn flags in preference to calling 'setSessionDynFlags',
--   since that function also reloads packages (which is very slow).
modifyDynFlags :: GhcMonad m => (DynFlags -> DynFlags) -> m ()
modifyDynFlags f = do
  newFlags <- f <$> getSessionDynFlags
  -- We do not use setSessionDynFlags here since we handle package
  -- initialization separately.
  modifySession $ \h ->
    h { hsc_dflags = newFlags, hsc_IC = (hsc_IC h) {ic_dflags = newFlags} }

-- | Given a 'UnitId' try and find the associated 'PackageConfig' in the environment.
lookupPackageConfig :: UnitId -> HscEnv -> Maybe UnitInfo
lookupPackageConfig unitId env =
    lookupUnit' False pkgConfigMap (preloadClosure (ue_units (hsc_unit_env env))) (RealUnit (Definite unitId))
    where
        pkgConfigMap =
            -- For some weird reason, the GHC API does not provide a way to get the PackageConfigMap
            -- from PackageState so we have to wrap it in DynFlags first.
            unitInfoMap $ ue_units (hsc_unit_env env)


-- | Convert from the @text@ package to the @GHC@ 'StringBuffer'.
--   Currently implemented somewhat inefficiently (if it ever comes up in a profile).
textToStringBuffer :: T.Text -> StringBuffer
textToStringBuffer = stringToStringBuffer . T.unpack

runParser :: DynFlags -> String -> P a -> ParseResult a
runParser flags str parser = unP parser parseState
    where
      filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      opts = initParserOpts flags
      parseState = initParserState opts buffer location

stringBufferToByteString :: StringBuffer -> ByteString
stringBufferToByteString StringBuffer{..} = PS buf cur len

bytestringToStringBuffer :: ByteString -> StringBuffer
bytestringToStringBuffer (PS buf cur len) = StringBuffer{..}

-- | Pretty print a GHC value using 'unsafeGlobalDynFlags '.
prettyPrint :: Outputable a => a -> String
prettyPrint = renderWithContext defaultSDocContext . ppr

-- | Pretty print a 'RdrName' wrapping operators in parens
printRdrName :: RdrName -> String
printRdrName name = renderWithContext defaultSDocContext $ parenSymOcc rn (ppr rn)
  where
    rn = rdrNameOcc name

-- | Pretty print a 'Name' wrapping operators in parens
printName :: Name -> String
printName = printRdrName . nameRdrName

-- | Run a 'Ghc' monad value using an existing 'HscEnv'. Sets up and tears down all the required
--   pieces, but designed to be more efficient than a standard 'runGhc'.
evalGhcEnv :: HscEnv -> Ghc b -> IO b
evalGhcEnv env act = snd <$> runGhcEnv env act

-- | Run a 'Ghc' monad value using an existing 'HscEnv'. Sets up and tears down all the required
--   pieces, but designed to be more efficient than a standard 'runGhc'.
runGhcEnv :: HscEnv -> Ghc a -> IO (HscEnv, a)
runGhcEnv env act = do
    filesToClean <- newIORef emptyFilesToClean
    dirsToClean <- newIORef mempty
    let dflags = (hsc_dflags env){filesToClean=filesToClean, dirsToClean=dirsToClean, useUnicode=True}
    ref <- newIORef env{hsc_dflags=dflags}
    res <- unGhc act (Session ref) `finally` do
        cleanTempFiles dflags
        cleanTempDirs dflags
    (,res) <$> readIORef ref

-- | Given a module location, and its parse tree, figure out what is the include directory implied by it.
--   For example, given the file @\/usr\/\Test\/Foo\/Bar.hs@ with the module name @Foo.Bar@ the directory
--   @\/usr\/Test@ should be on the include path to find sibling modules.
moduleImportPath :: NormalizedFilePath -> GHC.ModuleName -> Maybe FilePath
-- The call to takeDirectory is required since DAML does not require that
-- the file name matches the module name in the last component.
-- Once that has changed we can get rid of this.
moduleImportPath (takeDirectory . fromNormalizedFilePath -> pathDir) mn
    -- This happens for single-component modules since takeDirectory "A" == "."
    | modDir == "." = Just pathDir
    | otherwise = dropTrailingPathSeparator <$> stripSuffix modDir pathDir
  where
    -- A for module A.B
    modDir =
        takeDirectory $
        fromNormalizedFilePath $ toNormalizedFilePath' $
        moduleNameSlashes mn

-- | An 'HscEnv' with equality. Two values are considered equal
--   if they are created with the same call to 'newHscEnvEq'.
data HscEnvEq = HscEnvEq
    { envUnique :: !Unique
    , hscEnv :: !HscEnv
    , deps   :: [(UnitId, DynFlags)]
               -- ^ In memory components for this HscEnv
               -- This is only used at the moment for the import dirs in
               -- the DynFlags
    , envImportPaths :: Maybe [String]
        -- ^ If Just, import dirs originally configured in this env
        --   If Nothing, the env import dirs are unaltered
    }

-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEq :: FilePath -> HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEq cradlePath hscEnv0 deps = do
    envUnique <- newUnique
    let relativeToCradle = (takeDirectory cradlePath </>)
        hscEnv = removeImportPaths hscEnv0

    -- Canonicalize import paths since we also canonicalize targets
    importPathsCanon <-
      mapM canonicalizePath $ relativeToCradle <$> importPaths (hsc_dflags hscEnv0)
    let envImportPaths = Just importPathsCanon

    return HscEnvEq{..}

newHscEnvEqWithImportPaths :: Maybe [String] -> HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEqWithImportPaths envImportPaths hscEnv deps = do
    envUnique <- newUnique
    return HscEnvEq{..}

-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEqPreserveImportPaths
    :: HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEqPreserveImportPaths hscEnv deps = do
    let envImportPaths = Nothing
    envUnique <- newUnique
    return HscEnvEq{..}

-- | Unwrap the 'HscEnv' with the original import paths.
--   Used only for locating imports
hscEnvWithImportPaths :: HscEnvEq -> HscEnv
hscEnvWithImportPaths HscEnvEq{..}
    | Just imps <- envImportPaths
    = hscEnv{hsc_dflags = (hsc_dflags hscEnv){importPaths = imps}}
    | otherwise
    = hscEnv

removeImportPaths :: HscEnv -> HscEnv
removeImportPaths hsc = hsc{hsc_dflags = (hsc_dflags hsc){importPaths = []}}

instance Show HscEnvEq where
  show HscEnvEq{envUnique} = "HscEnvEq " ++ show (hashUnique envUnique)

instance Eq HscEnvEq where
  a == b = envUnique a == envUnique b

instance NFData HscEnvEq where
  rnf (HscEnvEq a b c d) = rnf (hashUnique a) `seq` b `seq` c `seq` rnf d

instance Hashable HscEnvEq where
  hashWithSalt s = hashWithSalt s . envUnique

-- Fake instance needed to persuade Shake to accept this type as a key.
-- No harm done as ghcide never persists these keys currently
instance Binary HscEnvEq where
  put _ = error "not really"
  get = error "not really"

-- | Read a UTF8 file, with lenient decoding, so it will never raise a decoding error.
readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 f = T.decodeUtf8With T.lenientDecode <$> BS.readFile f

-- | Convert from a 'CgGuts' to a 'CoreModule'.
cgGutsToCoreModule :: SafeHaskellMode -> CgGuts -> ModDetails -> CoreModule
cgGutsToCoreModule safeMode guts modDetails = CoreModule
    (cg_module guts)
    (md_types modDetails)
    (cg_binds guts)
    safeMode

-- | Convert a 'Fingerprint' to a 'ByteString' by copying the byte across.
--   Will produce an 8 byte unreadable ByteString.
fingerprintToBS :: Fingerprint -> BS.ByteString
fingerprintToBS (Fingerprint a b) = BS.unsafeCreate 8 $ \ptr -> do
    ptr <- pure $ castPtr ptr
    pokeElemOff ptr 0 a
    pokeElemOff ptr 1 b

-- | Take the 'Fingerprint' of a 'StringBuffer'.
fingerprintFromStringBuffer :: StringBuffer -> IO Fingerprint
fingerprintFromStringBuffer (StringBuffer buf len cur) =
    withForeignPtr buf $ \ptr -> fingerprintData (ptr `plusPtr` cur) len


-- | A slightly modified version of 'hDuplicateTo' from GHC.
--   Importantly, it avoids the bug listed in https://gitlab.haskell.org/ghc/ghc/merge_requests/2318.
hDuplicateTo' :: Handle -> Handle -> IO ()
hDuplicateTo' h1@(FileHandle path m1) h2@(FileHandle _ m2)  = do
 withHandle__' "hDuplicateTo" h2 m2 $ \h2_ -> do
   -- The implementation in base has this call to hClose_help.
   -- _ <- hClose_help h2_
   -- hClose_help does two things:
   -- 1. It flushes the buffer, we replicate this here
   _ <- flushWriteBuffer h2_ `catch` \(_ :: IOException) -> pure ()
   -- 2. It closes the handle. This is redundant since dup2 takes care of that
   -- but even worse it is actively harmful! Once the handle has been closed
   -- another thread is free to reallocate it. This leads to dup2 failing with EBUSY
   -- if it happens just in the right moment.
   withHandle_' "hDuplicateTo" h1 m1 $ \h1_ -> do
     dupHandleTo path h1 Nothing h2_ h1_ (Just handleFinalizer)
hDuplicateTo' h1@(DuplexHandle path r1 w1) h2@(DuplexHandle _ r2 w2)  = do
 withHandle__' "hDuplicateTo" h2 w2  $ \w2_ -> do
   _ <- hClose_help w2_
   withHandle_' "hDuplicateTo" h1 w1 $ \w1_ -> do
     dupHandleTo path h1 Nothing w2_ w1_ (Just handleFinalizer)
 withHandle__' "hDuplicateTo" h2 r2  $ \r2_ -> do
   _ <- hClose_help r2_
   withHandle_' "hDuplicateTo" h1 r1 $ \r1_ -> do
     dupHandleTo path h1 (Just w1) r2_ r1_ Nothing
hDuplicateTo' h1 _ =
  ioe_dupHandlesNotCompatible h1

-- | This is copied unmodified from GHC since it is not exposed.
dupHandleTo :: FilePath
            -> Handle
            -> Maybe (MVar Handle__)
            -> Handle__
            -> Handle__
            -> Maybe HandleFinalizer
            -> IO Handle__
dupHandleTo filepath h other_side
            _hto_@Handle__{haDevice=devTo}
            h_@Handle__{haDevice=dev} mb_finalizer = do
  flushBuffer h_
  case cast devTo of
    Nothing   -> ioe_dupHandlesNotCompatible h
    Just dev' -> do
      _ <- IODevice.dup2 dev dev'
      FileHandle _ m <- dupHandle_ dev' filepath other_side h_ mb_finalizer
      takeMVar m

-- | This is copied unmodified from GHC since it is not exposed.
-- Note the beautiful inline comment!
dupHandle_ :: (IODevice dev, BufferedIO dev, RawIO dev, Typeable dev) => dev
           -> FilePath
           -> Maybe (MVar Handle__)
           -> Handle__
           -> Maybe HandleFinalizer
           -> IO Handle
dupHandle_ new_dev filepath other_side _h_@Handle__{..} mb_finalizer = do
   -- XXX wrong!
  mb_codec <- if isJust haEncoder then fmap Just getLocaleEncoding else return Nothing
  mkHandle new_dev filepath haType True{-buffered-} mb_codec
      NewlineMode { inputNL = haInputNL, outputNL = haOutputNL }
      mb_finalizer other_side

-- | This is copied unmodified from GHC since it is not exposed.
ioe_dupHandlesNotCompatible :: Handle -> IO a
ioe_dupHandlesNotCompatible h =
   ioException (IOError (Just h) IllegalOperation "hDuplicateTo"
                "handles are incompatible" Nothing Nothing)
