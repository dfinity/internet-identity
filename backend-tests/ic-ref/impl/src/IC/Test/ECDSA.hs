-- Unit test for IC.Test.Crypto.ECDSA
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ViewPatterns #-}
module IC.Test.ECDSA (ecdsaTests) where

import qualified Data.ByteString.Lazy as BS

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.IO ()

import qualified IC.Crypto.ECDSA as ECDSA

ecdsaTests :: TestTree
ecdsaTests = testGroup "ECDSA crypto tests"
  [ testProperty "create-sign-verify" $
      \(BS.pack -> seed) (BS.pack -> msg) -> do
        let sk = ECDSA.createKey seed
        sig <- ECDSA.sign sk msg
        assertBool "verifies" $ ECDSA.verify (ECDSA.toPublicKey sk) msg sig
  , testProperty "invalid sig" $
      \(BS.pack -> seed) (BS.pack -> msg) (BS.pack -> sig) ->
        let sk = ECDSA.createKey seed in
        assertBool "does not verify" $ not $ ECDSA.verify (ECDSA.toPublicKey sk) msg sig
  , testProperty "wrong message" $
      \(BS.pack -> seed) (BS.pack -> msg1) (BS.pack -> msg2) ->
      msg1 /= msg2 ==> do
        let sk = ECDSA.createKey seed
        sig <- ECDSA.sign sk msg2
        assertBool "does not verify" $ not $ ECDSA.verify (ECDSA.toPublicKey sk) msg1 sig
  ]

