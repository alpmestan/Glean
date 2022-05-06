module Glean.Clang where

import Control.Monad.Except
import Control.Monad.Reader
import Options.Applicative
import System.Environment

import qualified Derive       as D
import qualified Derive.Lib   as D
import qualified Derive.Types as D
import Glean.Backend (withBackendWithDefaultOptions)
import Glean.CMake
import Glean.Util.ConfigProvider
import Util.EventBase (withEventBaseDataplane)

options :: ParserInfo GleanClangOpts
options = info (helper <*> parser) fullDesc
  where
    parser = GleanClangOpts <$> srcOpt <*> indexOpts <*> D.options <*> D.optionsPasses
    srcOpt =
      strOption
      ( long "srcdir"
     <> help "C++ sources directory, containing the root CMakeLists.txt"
     <> metavar "DIR"
      )
    indexOpts :: Parser CppIndexerOpts
    indexOpts = CppIndexerOpts
      <$> strOption
            ( long "target"
           <> short 't'
           <> help "CMake target to index from the project given by --srcdir"
           <> value "all"
           <> showDefault
            )
      <*> strOption
            ( long "clang-args"
           <> help "Extra arguments to pass to clang (in addition to the ones dictated by CMake)"
           <> value ""
           <> showDefault
            )

main :: IO ()
main = withConfigOptions options $ \(opts, cfgOpts) ->
  D.withNumCapabilities (D.cfgNumCapabilities $ deriverOpts opts) $
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \cfgAPI ->
  withBackendWithDefaultOptions evb cfgAPI (D.cfgService $ deriverOpts opts) $ \be ->
  do go opts be -- run indexer and write its data to glean DB
     D.runDerive (deriverOpts opts) (deriverPasses opts) be -- run deriving passes

  where go opts be = do
          r <-  runExceptT (flip runReaderT opts $ indexCMake be)
          case r of
            Left e  -> error $ "glean-clang error: " ++ show e
            Right _ -> putStrLn "OK"
