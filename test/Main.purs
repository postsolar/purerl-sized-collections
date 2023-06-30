module Test.Main where

import Effect (Effect)
import Test.Data.Vector as VectorTests
import Erl.Test.EUnit as EUnit

main :: Effect Boolean
main = do
  EUnit.runTests VectorTests.main
