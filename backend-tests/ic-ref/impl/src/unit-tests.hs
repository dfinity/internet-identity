{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import System.IO
import System.IO.Temp
import System.Directory
import qualified Data.Map as M

import qualified IC.Crypto.BLS as BLS
import IC.Ref
import IC.Types
import IC.Serialise ()
import IC.StateFile
import IC.Test.HashTree
import IC.Test.BLS
import IC.Test.WebAuthn
import IC.Test.ECDSA
import IC.Test.Secp256k1
import IC.HTTP.GenR
import IC.HTTP.RequestId

main :: IO ()
main = do
    BLS.init
    defaultMain tests

tests :: TestTree
tests = testGroup "ic-ref unit tests"
  [ testCase "Request id calculation from interface spec" $
     let gr = GRec $ mconcat
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob "\x00\x00\x00\x00\x00\x00\x04\xD2"
          , "method_name" =: GText "hello"
          , "arg" =: GBlob "DIDL\x00\xFD*"
          ]
      in requestId gr @?= "\x87\x81\x29\x1c\x34\x7d\xb3\x2a\x9d\x8c\x10\xeb\x62\xb7\x10\xfc\xe5\xa9\x3b\xe6\x76\x47\x4c\x42\xba\xbc\x74\xc5\x18\x58\xf9\x4b"
  , hashTreeTests
  , blsTests
  , webAuthnTests
  , ecdsaTests
  , secp256k1Tests
  , testGroup "State serialization"
    [ testCase "with file" $
      withSystemTempFile "ic-ref-unit-test.state" $ \fn h -> do
        -- start with an empty file
        hClose h
        removeFile fn

        -- Create the state
        withStore initialIC (Just fn) $ \store -> do
          modifyStore store $ submitRequest "dummyrequestid" $
            CallRequest (EntityId mempty) (EntityId "yay") "create_canister" "DIDL\0\0"

        -- now the file should exist
        doesFileExist fn  >>= assertBool "File exists"

        withStore initialIC (Just fn) $ \store -> do
          ic <- peekStore store
          assertBool "No canisters yet expected" (null (canisters ic))
          modifyStore store runToCompletion

        withStore initialIC (Just fn) $ \store -> do
          ic <- peekStore store
          case M.elems (canisters ic) of
            [] -> assertFailure "No canisters created"
            [CanState {controller}] -> controller @?= EntityId "yay"
            _ -> assertFailure "Too many canisters?"
    ]
  ]
