{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Runners.AntXML
import Test.Tasty.Runners.Html
import Test.Tasty.Runners

import IC.Test.Options
import IC.Test.Spec
import qualified IC.Crypto.BLS as BLS

main :: IO ()
main = do
    BLS.init
    os <- parseOptions ingredients (testGroup "dummy" [])
    tc <- preFlight os
    defaultMainWithIngredients ingredients (icTests tc)
  where
    ingredients =
      [ rerunningTests
        [ listingTests
        , includingOptions [endpointOption]
        , antXMLRunner `composeReporters` htmlRunner `composeReporters` consoleTestReporter
        ]
      ]
