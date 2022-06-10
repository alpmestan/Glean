{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
module Glean.Clang.Test (driver, driverWith) where

import Glean.Indexer
import Glean.Indexer.Cpp
import Glean.Regression.Snapshot.Driver

driverWith :: Bool -> Driver Clang
driverWith deriveToo = driverFromIndexer indexer'
  where
    baseIndexer = indexerWith deriveToo
    withCompileCommandsFor opts params f = do
      putStrLn ("opts: " ++ show opts)
      putStrLn ("params: " ++ show params)
      f "/home/nawak"
      putStrLn "-------"
    indexer' = baseIndexer
      { indexerRun = \clang backend repo params ->
          withCompileCommandsFor clang params $ \cdbDir ->
            indexerRun baseIndexer
                       (clang { clangCompileDBDir = Just cdbDir })
                       backend
                       repo
                       params
      }

driver :: Driver Clang
driver = driverWith False
