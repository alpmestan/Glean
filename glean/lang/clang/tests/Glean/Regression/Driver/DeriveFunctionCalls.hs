{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.DeriveFunctionCalls (main) where

import Derive.Lib (DerivePass(..))
import Glean.Clang.Test.DerivePass (driver)
import Glean.Regression.Test (testMain)

main :: IO ()
main = testMain $ driver [DeriveFunctionCalls, DeriveFunctionCalls_Pass_2]
