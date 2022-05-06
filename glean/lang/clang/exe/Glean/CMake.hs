module Glean.CMake where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Proxy
import Data.Set (Set)
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process
import Thrift.Protocol (deserializeGen)
import Thrift.Protocol.Compact (Compact)

import Glean (Backend, sendBatch)
import Glean.LocalOrRemote (serializeInventory)


import qualified Data.ByteString as BS
import qualified Derive          as D
import qualified Derive.Lib      as D
import qualified Derive.Types    as D

data GleanClangOpts = GleanClangOpts
  { sourceDir     :: FilePath
  , indexerOpts   :: CppIndexerOpts
  , deriverOpts   :: D.Config
  , deriverPasses :: Set D.DerivePass
  }

data CppIndexerOpts = CppIndexerOpts
  { cppCMakeTarget :: String       -- ^ cmake target to focus on (defaut is @all])
  , cppClangArgs   :: String       -- ^ extra clang arguments (default is @""@)
  } deriving Show

data IndexError
  = CMakeCmdError String Int String String -- ^ cmake command, exit code, stdout, stderr
  | CMakeCommandsFileMissing FilePath -- ^ @compile_commands.json@ missing
  | IndexerCmdError String Int String String -- ^ indexer command, exit code, stdout, stderr
  | GleanCmdError String Int String String -- ^ @glean@ command, exit code, stdout, stderr
  | DataWriteError String -- ^ couldn't decode the C++ indexer's binary file
  deriving Show

type IndexM = ReaderT GleanClangOpts (ExceptT IndexError IO)

-- | Generate @compile_commands.json@ file for a CMake project
generateBuildCommands
  :: FilePath        -- ^ path to (temporary) cmake build dir
  -> IndexM FilePath -- ^ path to @compile_commands.jsonà file, when successful
generateBuildCommands buildDir = do
  srcDir    <- getCppSrcDir
  clangArgs <- getClangArgs
  let args = words clangArgs
          ++ [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
             , "-S", srcDir
             , "-B", buildDir
             ]
  (ex, out, err) <- liftIO (readProcessWithExitCode "cmake" args "")
  case ex of
    ExitSuccess   -> return (buildDir </> "compile_commands.json")
    ExitFailure i -> throwError $ CMakeCmdError (unwords ("cmake":args)) i out err

runIndexer
  :: FilePath     -- ^ path to (temporary) cmake build dir
  -> FilePath     -- ^ path to file where indexer data is dumped
  -> FilePath     -- ^ path to schema inventory file
  -> IndexM ()
runIndexer buildDir indexerFile inventoryFile = do
  cmakeTarget <- getCMakeTarget
  srcDir      <- getCppSrcDir
  let args = [ "-cdb_dir", buildDir
             , "-cdb_target", cmakeTarget
             , "-root", srcDir
             , "-dump", indexerFile
             , "--inventory", inventoryFile
             ]

  liftIO . putStrLn $
    "calling:\t " ++ unwords ("glean-clang-index":args)
  (ex, out, err) <- liftIO $ readProcessWithExitCode "glean-clang-index" args ""
  case ex of
    ExitSuccess   -> return ()
    ExitFailure i -> throwError $ IndexerCmdError (unwords ("glean-clang-index":args)) i out err

writeInventory
  :: Backend be
  => be
  -> FilePath -- ^ desired path to the inventory data file (to be written)
  -> IndexM ()
writeInventory backend outFile = do
  repo <- asks (D.cfgRepo . deriverOpts)
  liftIO $ serializeInventory backend repo
       >>= BS.writeFile outFile

writeToDB
  :: Backend be
  => be
  -> FilePath -- ^ path to indexed data (binary format)
  -> IndexM ()
writeToDB backend dataFile = do
  repo <- asks (D.cfgRepo . deriverOpts)
  handleAll (\e -> throwError $ DataWriteError $ dataFile <> ": " <> show e) $ do
    dat <- liftIO (BS.readFile dataFile)
    batch <- case deserializeGen (Proxy :: Proxy Compact) dat of
      Left parseError -> throwError (DataWriteError parseError)
      Right result    -> return result
    void . liftIO $ sendBatch backend repo batch

indexCMake :: Backend be => be -> IndexM ()
indexCMake backend =
  withSystemTempDirectory "glean-cmake" $ \tmpDir -> do
    compileCommandsPath <- generateBuildCommands tmpDir
    -- Uncomment the 5 lines below to see what happens if we additionally try to
    -- build the project.
    -- (ex, out, err) <- liftIO $
    --   readProcessWithExitCode "cmake" ["--build", tmpDir] ""
    -- liftIO $ do
    --   putStrLn ("stdout:\n" ++ out ++ "\n---")
    --   putStrLn ("stderr:\n" ++ err ++ "\n---")

    exists <- liftIO (doesFileExist compileCommandsPath)
    when (not exists) $
      throwError (CMakeCommandsFileMissing compileCommandsPath)
    let inventoryFile = tmpDir </> "inventory.data"
        indexerFile   = tmpDir </> "indexer.data"
    writeInventory backend inventoryFile
    runIndexer tmpDir indexerFile inventoryFile
    writeToDB backend indexerFile

-- utilities

getCMakeTarget :: IndexM String
getCMakeTarget = asks (cppCMakeTarget . indexerOpts)

getCppSrcDir :: IndexM FilePath
getCppSrcDir = asks sourceDir

getClangArgs :: IndexM String
getClangArgs = asks (cppClangArgs . indexerOpts)
