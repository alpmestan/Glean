module Glean.CMake where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import GHC.Generics
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HM
import qualified Text.Parsec          as Parsec

data IndexError
  = CMakeError Int String String -- ^ cmake exit code, stdout, stderr
  | CMakeCommandsFileMissing FilePath -- ^ @compile_commands.json@ missing
  | CMakeCommandsDecodingError String -- ^ @compile_commands.json@ decoding error
  | CMakeCommandParseError Parsec.ParseError -- ^ one of the @compile_commands.json@
                                             --   commands couldn't be parsed
  deriving Show

type IndexM = ExceptT IndexError IO

-- | Generate @compile_commands.json@ file for a CMake project
generateBuildCommands
  :: FilePath -- ^ path to directory with the root `CMakeLists.txt` and source files
  -> FilePath -- ^ build dir, where @compile_commands.json@ will be generated
  -> [String] -- ^ extra arguments to pass to CMake
  -> IndexM FilePath -- ^ @(exit code, stdout, stderr)@ in case of failure
generateBuildCommands srcDir buildDir extraArgs = do
  -- liftIO . putStrLn $ "Running: " ++ unwords ("cmake":args)
  (ex, out, err) <- liftIO (readProcessWithExitCode "cmake" args "")
  case ex of
    ExitSuccess   -> return (buildDir </> "compile_commands.json")
    ExitFailure i -> throwError $ CMakeError i out err

  where args = extraArgs
            ++ [ "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
               , "-S", srcDir, "-B", buildDir
               ]

data CompileCommand = CompileCommand
  { directory :: FilePath
  , command   :: String
  , file      :: FilePath
  } deriving (Generic, Show)

instance Aeson.FromJSON CompileCommand

type CompileCommands = [CompileCommand]

runIndexer
  :: String       -- ^ repo name
  -> FilePath     -- ^ path to gleandb dir
  -> FilePath     -- ^ path to source dir
  -> FilePath     -- ^ path to cmake build dir
  -> Maybe String -- ^ cmake target, default: all
  -> IndexM ()
runIndexer repoName gleandbDir srcDir buildDir mtarget = do
  liftIO . putStrLn $
    "calling:\t " ++ unwords ("cabal":args)
  (ex, out, err) <- liftIO $ readProcessWithExitCode "cabal" args ""
  liftIO $ do
    print ex
    putStrLn "---"
    putStrLn out
    putStrLn "---"
    putStrLn err
    putStrLn "---"

  where args = [ "--project-file=ci.cabal.project"
               , "run", "glean-clang-index"
               , "--"
               , "-cdb_dir", buildDir
               , "-cdb_target", fromMaybe "all" mtarget
               , "-root", srcDir
               , "-dump", gleandbDir
               , "--inventory", "/Glean/inventory.data"
               ]

indexCMake :: FilePath -> FilePath -> [String] -> IndexM ()
indexCMake srcDir gleandbDir extraArgs =
  withSystemTempDirectory "glean-cmake" $ \tmpDir -> do
    compileCommandsPath <- generateBuildCommands srcDir tmpDir extraArgs
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
    runIndexer "tmp" gleandbDir srcDir tmpDir Nothing
    -- decodeRes <- Aeson.eitherDecode' <$> liftIO (LBS.readFile compileCommandsPath)
    -- case decodeRes of
    --   Left err   -> throwError (CMakeCommandsDecodingError err)
    --   Right cmds -> commandsToMap cmds

type ArgumentMap = HashMap FilePath [String]

commandsToMap :: CompileCommands -> IndexM ArgumentMap
commandsToMap cmds = HM.fromList <$> traverse cmdArguments cmds

  where cmdArguments :: CompileCommand -> IndexM (FilePath, [String])
        cmdArguments cmd = do
          let srcFile = file cmd
          args <- parseArgs (file cmd) (command cmd)
          return (srcFile, args)

parseArgs :: FilePath -> String -> IndexM [String]
parseArgs file cmd = case Parsec.runParser tokens () (file ++ " command") cmd of
  Left parseError -> throwError (CMakeCommandParseError parseError)
  Right xs        -> return xs

  where tokens :: Parsec.Parsec String () [String]
        tokens = Parsec.manyTill (token >>= \t -> Parsec.skipMany Parsec.space >> return t) Parsec.eof

        token :: Parsec.Parsec String () String
        token     = Parsec.between (Parsec.char '"') (Parsec.char '"')
                      (Parsec.many $ Parsec.noneOf "\"")
                <|> Parsec.between (Parsec.char '\'') (Parsec.char '\'')
                      (Parsec.many $ Parsec.noneOf "\'")
                <|> Parsec.many1 (Parsec.noneOf " \t")
