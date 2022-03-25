module Glean.Clang where

import Control.Monad.Except
import Glean.CMake
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    srcDir:gleandbDir:extraArgs -> go srcDir gleandbDir extraArgs
    _ -> usage

  where usage = error "usage: glean-clang <source dir to index> <gleandb dir> [extra arguments to pass to clang...]"
        go srcDir gleandbDir extraArgs = do
          r <- runExceptT (indexCMake srcDir gleandbDir extraArgs)
          putStrLn $ case r of
            Left e  -> "Indexing error: " ++ show e
            Right _ -> "Indexing done."
