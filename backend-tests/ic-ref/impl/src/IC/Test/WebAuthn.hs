-- Unit test for IC.Test.Crypto.WebAuthn
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE ViewPatterns #-}
module IC.Test.WebAuthn (webAuthnTests) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.QuickCheck.IO ()

import qualified IC.Crypto.WebAuthn as WebAuthn

assertRight :: Either T.Text () -> Assertion
assertRight (Right ()) = return ()
assertRight (Left err) = assertFailure (T.unpack err)

assertLeft :: Either T.Text () -> Assertion
assertLeft (Left _) = return ()
assertLeft (Right _) = assertFailure "Unexpected success"

webAuthnTests :: TestTree
webAuthnTests = testGroup "WebAuthn crypto tests"
  [ testProperty "create-sign-verify" $
      \(BS.pack -> seed) (BS.pack -> msg) -> do
        let sk = WebAuthn.createKey seed
        sig <- WebAuthn.sign sk msg
        assertRight $ WebAuthn.verify (WebAuthn.toPublicKey sk) msg sig
  , testProperty "invalid sig" $
      \(BS.pack -> seed) (BS.pack -> msg) (BS.pack -> sig) ->
        let sk = WebAuthn.createKey seed in
        assertLeft $ WebAuthn.verify (WebAuthn.toPublicKey sk) msg sig
  , testProperty "wrong message" $
      \(BS.pack -> seed) (BS.pack -> msg1) (BS.pack -> msg2) ->
      msg1 /= msg2 ==> do
        let sk = WebAuthn.createKey seed
        sig <- WebAuthn.sign sk msg2
        assertLeft $ WebAuthn.verify (WebAuthn.toPublicKey sk) msg1 sig
  ]

