{- |

This module contains a test suite for the Internet Computer

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Test.Spec (preFlight, TestConfig, connect, ReplWrapper(..), icTests) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Text.Hex as H
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Numeric.Natural
import Data.List
import Data.Char
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import Control.Monad.Trans
import Control.Concurrent
import Control.Monad
import Control.Exception (catch)
import Data.Traversable
import Data.Word
import Data.Functor
import GHC.TypeLits
import System.FilePath
import System.Directory
import System.Environment
import System.Random
import System.Exit
import Data.Time.Clock.POSIX
import Codec.Candid (Principal(..), prettyPrincipal)
import qualified Data.Binary.Get as Get
import qualified Codec.Candid as Candid
import Data.Row
import qualified Data.Row.Records as R
import qualified Data.Row.Variants as V
import qualified Data.Row.Internal as R

import IC.Types (EntityId(..))
import IC.Version
import IC.HTTP.GenR
import IC.HTTP.GenR.Parse
import IC.HTTP.CBOR (decode, encode)
import IC.HTTP.RequestId
import IC.Management
import IC.Crypto
import qualified IC.Crypto.CanisterSig as CanisterSig
import qualified IC.Crypto.DER as DER
import qualified IC.Crypto.DER_BLS as DER_BLS
import IC.Id.Forms hiding (Blob)
import IC.Test.Options
import IC.Test.Universal
import IC.HashTree hiding (Blob, Label)
import IC.Certificate
import IC.Certificate.Value
import IC.Certificate.CBOR
import IC.Hash

-- * Test data, standard requests

doesn'tExist :: Blob
doesn'tExist = "\xDE\xAD\xBE\xEF" -- hopefully no such canister/user exists

defaultSK :: SecretKey
defaultSK = createSecretKeyEd25519 "fixed32byteseedfortesting"

otherSK :: SecretKey
otherSK = createSecretKeyEd25519 "anotherfixed32byteseedfortesting"

webAuthnSK :: SecretKey
webAuthnSK = createSecretKeyWebAuthn "webauthnseed"

ecdsaSK :: SecretKey
ecdsaSK = createSecretKeyECDSA "ecdsaseed"

secp256k1SK :: SecretKey
secp256k1SK = createSecretKeySecp256k1 "secp256k1seed"

defaultUser :: Blob
defaultUser = mkSelfAuthenticatingId $ toPublicKey defaultSK
otherUser :: Blob
otherUser = mkSelfAuthenticatingId $ toPublicKey otherSK
webAuthnUser :: Blob
webAuthnUser = mkSelfAuthenticatingId $ toPublicKey webAuthnSK
ecdsaUser :: Blob
ecdsaUser = mkSelfAuthenticatingId $ toPublicKey ecdsaSK
secp256k1User :: Blob
secp256k1User = mkSelfAuthenticatingId $ toPublicKey secp256k1SK
anonymousUser :: Blob
anonymousUser = "\x04"

queryToNonExistant :: GenR
queryToNonExistant = rec
    [ "request_type" =: GText "query"
    , "sender" =: GBlob anonymousUser
    , "canister_id" =: GBlob doesn'tExist
    , "method_name" =: GText "foo"
    , "arg" =: GBlob "nothing to see here"
    ]

readStateEmpty :: GenR
readStateEmpty = rec
    [ "request_type" =: GText "read_state"
    , "sender" =: GBlob defaultUser
    , "paths" =: GList []
    ]

trivialWasmModule :: Blob
trivialWasmModule = "\0asm\1\0\0\0"

addIfNotThere :: Monad m => T.Text -> m GenR -> GenR -> m GenR
addIfNotThere f _ (GRec hm)| f `HM.member` hm = return (GRec hm)
addIfNotThere f a (GRec hm) = do
  x <- a
  return $ GRec $ HM.insert f x hm
addIfNotThere _ _ _ = error "addIfNotThere: not a record"

deleteField :: T.Text -> GenR -> GenR
deleteField f (GRec hm) = GRec $ HM.delete f hm
deleteField _ _ = error "deleteField: not a record"

modNatField :: T.Text -> (Natural -> Natural) -> GenR -> GenR
modNatField f g (GRec hm) = GRec $ HM.adjust underNat f hm
  where underNat :: GenR -> GenR
        underNat (GNat n) = GNat (g n)
        underNat _ = error "modNatField: not a nat field"
modNatField _ _ _ = error "modNatField: not a record"

addNonce :: GenR -> IO GenR
addNonce = addIfNotThere "nonce" $
    GBlob <$> getRand8Bytes

-- Adds expiry 1 minute
addExpiry :: GenR -> IO GenR
addExpiry = addIfNotThere "ingress_expiry" $ do
    t <- getPOSIXTime
    return $ GNat $ round ((t + 60) * 1000_000_000)

envelopeFor :: Blob -> GenR -> IO GenR
envelopeFor u content | u == anonymousUser = return $ rec [ "content" =: content ]
envelopeFor u content = envelope key content
  where
    key ::  SecretKey
    key | u == defaultUser = defaultSK
        | u == otherUser = otherSK
        | u == webAuthnUser = webAuthnSK
        | u == ecdsaUser = ecdsaSK
        | u == secp256k1User = secp256k1SK
        | u == anonymousUser = error "No key for the anonymous user"
        | otherwise = error $ "Don't know key for user " ++ show u

envelope :: SecretKey -> GenR -> IO GenR
envelope sk = delegationEnv sk []

delegationEnv :: SecretKey -> [(SecretKey, Maybe [Blob])] -> GenR -> IO GenR
delegationEnv sk1 dels content = do
    let sks = sk1 : map fst dels

    t <- getPOSIXTime
    let expiry = round ((t + 60) * 1000_000_000)
    delegations <- for (zip sks dels) $ \(sk1, (sk2,targets)) -> do
      let delegation = rec $
            [ "pubkey" =: GBlob (toPublicKey sk2)
            , "expiration" =: GNat expiry
            ] ++
            [ "targets" =: GList (map GBlob ts) | Just ts <- pure targets ]
      sig <- sign "ic-request-auth-delegation" sk1 (requestId delegation)
      return $ rec [ "delegation" =: delegation, "signature" =: GBlob sig ]
    sig <- sign "ic-request" (last sks) (requestId content)
    return $ rec $
      [ "sender_pubkey" =: GBlob (toPublicKey sk1)
      , "sender_sig" =: GBlob sig
      , "content" =: content
      ] ++
      [ "sender_delegation" =: GList delegations | not (null delegations) ]

-- a little bit of smartness in our combinators

senderOf :: GenR -> Blob
senderOf (GRec hm) | Just (GBlob id) <- HM.lookup "sender" hm = id
senderOf _ = anonymousUser

addNonceExpiryEnv :: GenR -> IO GenR
addNonceExpiryEnv req = do
  addNonce req >>= addExpiry >>= envelopeFor (senderOf req)

badEnvelope :: GenR -> GenR
badEnvelope content = rec
    [ "sender_pubkey" =: GBlob (toPublicKey defaultSK)
    , "sender_sig" =: GBlob (BS.replicate 64 0x42)
    , "content" =: content
    ]

noDomainSepEnv :: SecretKey -> GenR -> IO GenR
noDomainSepEnv sk content = do
  sig <- sign "" sk (requestId content)
  return $ rec
    [ "sender_pubkey" =: GBlob (toPublicKey sk)
    , "sender_sig" =: GBlob sig
    , "content" =: content
    ]

noExpiryEnv, pastExpiryEnv, futureExpiryEnv :: GenR -> GenR
noExpiryEnv = deleteField "ingress_expiry"
pastExpiryEnv = modNatField "ingress_expiry" (subtract 3600_000_000_000)
futureExpiryEnv = modNatField "ingress_expiry" (+ 3600_000_000_000)

-- * Preflight checks: Get the root key, and tell user about versions

data TestConfig = TestConfig
    { tc_root_key :: Blob
    , tc_manager :: Manager
    , tc_endPoint :: String
    }

makeTestConfig :: String -> IO TestConfig
makeTestConfig ep' = do
    manager <- newTlsManagerWith $ tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 60_000_000 -- 60s
      }
    request <- parseRequest $ ep ++ "/api/v2/status"
    putStrLn $ "Fetching endpoint status from " ++ show ep ++ "..."
    s <- (httpLbs request manager >>= okCBOR >>= statusResonse)
        `catch` (\(HUnitFailure _ r) -> putStrLn r >> exitFailure)

    putStrLn $ "Spec version tested:  " ++ T.unpack specVersion
    putStrLn $ "Spec version claimed: " ++ T.unpack (status_api_version s)

    return TestConfig
        { tc_root_key = status_root_key s
        , tc_manager = manager
        , tc_endPoint = ep
        }
  where
    -- strip trailing slash
    ep | null ep'        = error "empty endpoint"
       | last ep' == '/' = init ep'
       | otherwise       = ep'

preFlight :: OptionSet -> IO TestConfig
preFlight os = do
    let Endpoint ep = lookupOption os
    makeTestConfig ep


newtype ReplWrapper = R (forall a. (HasTestConfig => a) -> a)
-- |  This is for use from the Haskell REPL, see README.md
connect :: String -> IO ReplWrapper
connect ep = do
    testConfig <- makeTestConfig ep
    let ?testConfig = testConfig
    return (R id)


-- * The actual test suite (see below for helper functions)

icTests :: TestConfig -> TestTree
icTests = withTestConfig $ testGroup "Interface Spec acceptance tests"
  [ simpleTestCase "create and install" $ \_ ->
      return ()

  , testCase "create_canister necessary" $
      ic_install'' defaultUser (enum #install) doesn'tExist trivialWasmModule ""
          >>= isErrOrReject [3,5]

  , testCaseSteps "management requests" $ \step -> do
      step "Create (provisional)"
      can_id <- create

      step "Install"
      ic_install ic00 (enum #install) can_id trivialWasmModule ""

      step "Install again fails"
      ic_install'' defaultUser (enum #install) can_id trivialWasmModule ""
        >>= isErrOrReject [3,5]

      step "Reinstall"
      ic_install ic00 (enum #reinstall) can_id trivialWasmModule ""

      step "Reinstall as wrong user"
      ic_install'' otherUser (enum #reinstall) can_id trivialWasmModule ""
        >>= isErrOrReject [3,5]

      step "Upgrade"
      ic_install ic00 (enum #upgrade) can_id trivialWasmModule ""

      step "Upgrade as wrong user"
      ic_install'' otherUser (enum #upgrade) can_id trivialWasmModule ""
        >>= isErrOrReject [3,5]

      step "Change controller"
      ic_set_controller ic00 can_id otherUser

      step "Change controller (with wrong controller)"
      ic_set_controller'' defaultUser can_id otherUser
        >>= isErrOrReject [3,5]

      step "Reinstall as new controller"
      ic_install (ic00as otherUser) (enum #reinstall) can_id trivialWasmModule ""

  , testCaseSteps "reinstall on empty" $ \step -> do
      step "Create"
      can_id <- create

      step "Reinstall over empty canister"
      ic_install ic00 (enum #reinstall) can_id trivialWasmModule ""

  , testCaseSteps "canister_status" $ \step -> do
      step "Create empty"
      cid <- create
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running
      cs .! #settings .! #controller @?= Principal defaultUser
      cs .! #module_hash @?= Nothing

      step "Install"
      ic_install ic00 (enum #install) cid trivialWasmModule ""
      cs <- ic_canister_status ic00 cid
      cs .! #module_hash @?= Just (sha256 trivialWasmModule)

  , testCaseSteps "canister lifecycle" $ \step -> do
      cid <- install $
        onPreUpgrade $ callback $
          ignore (stableGrow (int 1)) >>>
          stableWrite (int 0) (i2b getStatus)

      step "Is running (via management)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running

      step "Is running (local)?"
      query cid (replyData (i2b getStatus)) >>= asWord32 >>= is 1

      step "Stop"
      ic_stop_canister ic00 cid

      step "Is stopped (via management)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #stopped

      step "Stop is noop"
      ic_stop_canister ic00 cid

      step "Cannot call (update)?"
      call'' cid reply >>= isErrOrReject [5]

      step "Cannot call (query)?"
      query' cid reply >>= isReject [5]

      step "Upgrade"
      upgrade cid $ setGlobal (i2b getStatus)

      step "Start canister"
      ic_start_canister ic00 cid

      step "Is running (via managemnet)?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running

      step "Is running (local)?"
      query cid (replyData (i2b getStatus)) >>= asWord32 >>= is 1

      step "Was stopped during pre-upgrade?"
      query cid (replyData (stableRead (int 0) (int 4))) >>= asWord32 >>= is 3

      step "Was stopped during post-upgrade?"
      query cid (replyData getGlobal) >>= asWord32 >>= is 3

      step "Can call (update)?"
      call_ cid reply

      step "Can call (query)?"
      query_ cid reply

      step "Start is noop"
      ic_start_canister ic00 cid

  , testCaseSteps "canister deletion" $ \step -> do
      cid <- install noop

      step "Deletion fails"
      ic_delete_canister' ic00 cid >>= isReject [5]

      step "Stop"
      ic_stop_canister ic00 cid

      step "Is stopped?"
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #stopped

      step "Deletion succeeds"
      ic_delete_canister ic00 cid

      -- Disabled; such a call gets accepted (200) but
      -- then the status never shows up, which causes a timeout
      --
      -- step "Cannot call (update)?"
      -- call' cid reply >>= isReject [3]

      step "Cannot call (inter-canister)?"
      cid2 <- install noop
      do call cid2 $ inter_update cid defArgs
        >>= isRelay >>= isReject [3]

      step "Cannot call (query)?"
      query' cid reply >>= isReject [3]

      step "Cannot query canister_status"
      ic_canister_status'' defaultUser cid >>= isErrOrReject [3,5]

      step "Deletion fails"
      ic_delete_canister'' defaultUser cid >>= isErrOrReject [3,5]


  , testCaseSteps "canister lifecycle (wrong controller)" $ \step -> do
      cid <- install noop

      step "Start as wrong user"
      ic_start_canister'' otherUser cid >>= isErrOrReject [3,5]
      step "Stop as wrong user"
      ic_stop_canister'' otherUser cid >>= isErrOrReject [3,5]
      step "Canister Status as wrong user"
      ic_canister_status'' otherUser cid >>= isErrOrReject [3,5]
      step "Delete as wrong user"
      ic_delete_canister'' otherUser cid >>= isErrOrReject [3,5]


  , testCaseSteps "aaaaa-aa (inter-canister)" $ \step -> do
    -- install universal canisters to proxy the requests
    cid <- install noop
    cid2 <- install noop

    step "Create"
    can_id <- ic_provisional_create (ic00via cid) Nothing empty

    step "Install"
    ic_install (ic00via cid) (enum #install) can_id trivialWasmModule ""

    step "Install again fails"
    ic_install' (ic00via cid) (enum #install) can_id trivialWasmModule ""
      >>= isReject [3,5]

    step "Reinstall"
    ic_install (ic00via cid) (enum #reinstall) can_id trivialWasmModule ""

    step "Reinstall as wrong user"
    ic_install' (ic00via cid2) (enum #reinstall) can_id trivialWasmModule ""
      >>= isReject [3,5]

    step "Upgrade"
    ic_install (ic00via cid) (enum #upgrade) can_id trivialWasmModule ""

    step "Change controller"
    ic_set_controller (ic00via cid) can_id cid2

    step "Change controller (with wrong controller)"
    ic_set_controller' (ic00via cid) can_id cid2
      >>= isReject [3,5]

    step "Reinstall as new controller"
    ic_install (ic00via cid2) (enum #reinstall) can_id trivialWasmModule ""

    step "Create"
    can_id2 <- ic_provisional_create (ic00via cid) Nothing empty

    step "Reinstall on empty"
    ic_install (ic00via cid) (enum #reinstall) can_id2 trivialWasmModule ""

  , simpleTestCase "aaaaa-aa (inter-canister, large)" $ \cid -> do
    universal_wasm <- getTestWasm "universal_canister"
    can_id <- ic_provisional_create (ic00via cid) Nothing empty
    ic_install (ic00via cid) (enum #install) can_id universal_wasm ""
    do call can_id $ replyData "Hi"
      >>= is "Hi"

  , simpleTestCase "randomness" $ \cid -> do
    r1 <- ic_raw_rand (ic00via cid)
    r2 <- ic_raw_rand (ic00via cid)
    BS.length r1 @?= 32
    BS.length r2 @?= 32
    assertBool "random blobs are different" $ r1 /= r2

  , testGroup "simple calls"
    [ simpleTestCase "Call" $ \cid ->
      call cid (replyData "ABCD") >>= is "ABCD"

    , simpleTestCase "Call (query)" $ \cid -> do
      query cid (replyData "ABCD") >>= is "ABCD"

    , simpleTestCase "Call no non-existant update method" $ \cid ->
      do awaitCall' cid $ rec
          [ "request_type" =: GText "call"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]
        >>= isErrOrReject [3]

    , simpleTestCase "Call no non-existant query method" $ \cid ->
      do queryCBOR cid $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob defaultUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "no_such_update"
          , "arg" =: GBlob ""
          ]
        >>= queryResponse >>= isReject [3]

    , simpleTestCase "reject" $ \cid ->
      call' cid (reject "ABCD") >>= isReject [4]

    , simpleTestCase "reject (query)" $ \cid ->
      query' cid (reject "ABCD") >>= isReject [4]

    , simpleTestCase "No response" $ \cid ->
      call' cid noop >>= isReject [5]

    , simpleTestCase "No response does not rollback" $ \cid -> do
      call'' cid (setGlobal "FOO") >>= isErrOrReject [5]
      query cid (replyData getGlobal) >>= is "FOO"

    , simpleTestCase "No response (query)" $ \cid ->
      query' cid noop >>= isReject [5]

    , simpleTestCase "Double reply" $ \cid ->
      call' cid (reply >>> reply) >>= isReject [5]

    , simpleTestCase "Double reply (query)" $ \cid ->
      query' cid (reply >>> reply) >>= isReject [5]

    , simpleTestCase "Reply data append after reply" $ \cid ->
      call' cid (reply >>> replyDataAppend "foo") >>= isReject [5]

    , simpleTestCase "Reply data append after reject" $ \cid ->
      call' cid (reject "bar" >>> replyDataAppend "foo") >>= isReject [5]

    , simpleTestCase "Caller" $ \cid ->
      call cid (replyData caller) >>= is defaultUser

    , simpleTestCase "Caller (query)" $ \cid ->
      query cid (replyData caller) >>= is defaultUser
    ]

  , testGroup "Settings"
    [ testCase "Create with controller set (provisional)" $ do
        cid <- ic_provisional_create ic00 Nothing (#controller .== Principal otherUser)
        cs <- ic_canister_status (ic00as otherUser) cid
        cs .! #settings .! #controller @?= Principal otherUser
        ic_set_controller'' defaultUser cid defaultUser >>= isErrOrReject [3,5]
        ic_set_controller (ic00as otherUser) cid defaultUser
        cs <- ic_canister_status (ic00as defaultUser) cid
        cs .! #settings .! #controller @?= Principal defaultUser

    , simpleTestCase "Create with controller set (aaaaa-aa)" $ \cid -> do
        cid2 <- ic_create (ic00viaWithCycles cid 20_000_000_000_000) empty
        cs <- ic_canister_status (ic00via cid) cid2
        cs .! #settings .! #controller @?= Principal cid
        ic_set_controller'' defaultUser cid2 defaultUser >>= isErrOrReject [3,5]
        ic_set_controller (ic00via cid) cid2 defaultUser
        cs <- ic_canister_status (ic00as defaultUser) cid2
        cs .! #settings .! #controller @?= Principal defaultUser

    , simpleTestCase "Valid allocations" $ \cid -> do
        cid2 <- ic_create (ic00viaWithCycles cid 20_000_000_000_000) $ empty
          .+ #compute_allocation .== (1::Natural)
          .+ #memory_allocation .== (1024*1024::Natural)
          .+ #freezing_threshold .== 1000_000
        cs <- ic_canister_status (ic00via cid) cid2
        cs .! #settings .! #compute_allocation @?= 1
        cs .! #settings .! #memory_allocation @?= 1024*1024
        cs .! #settings .! #freezing_threshold @?= 1000_000

    , testGroup "via provisional_create_canister_with_cycles:"
        [ testCase "Invalid compute allocation" $ do
            ic_provisional_create' ic00 Nothing (#compute_allocation .== 101)
               >>= isReject [3,5]
        , testCase "Invalid memory allocation (2^49)" $ do
            ic_provisional_create' ic00 Nothing (#compute_allocation .== 2^(49::Int))
               >>= isReject [3,5]
        , testCase "Invalid memory allocation (2^70)" $ do
            ic_provisional_create' ic00 Nothing (#compute_allocation .== 2^(70::Int))
               >>= isReject [3,5]
        , testCase "Invalid freezing threshold (2^70)" $ do
            ic_provisional_create' ic00 Nothing (#freezing_threshold .== 2^(70::Int))
               >>= isReject [3,5]
        ]
    , testGroup "via create_canister:"
        [ simpleTestCase "Invalid compute allocation" $ \cid -> do
            ic_create' (ic00via cid) (#compute_allocation .== 101)
               >>= isReject [3,5]
        , simpleTestCase "Invalid memory allocation (2^49)" $ \cid -> do
            ic_create' (ic00via cid) (#compute_allocation .== 2^(49::Int))
               >>= isReject [3,5]
        , simpleTestCase "Invalid memory allocation (2^70)" $ \cid -> do
            ic_create' (ic00via cid) (#compute_allocation .== 2^(70::Int))
               >>= isReject [3,5]
        , simpleTestCase "Invalid freezing threshold (2^70)" $ \cid -> do
            ic_create' (ic00via cid) (#freezing_threshold .== 2^(70::Int))
               >>= isReject [3,5]
        ]
    , testGroup "via update_settings"
        [ simpleTestCase "Invalid compute allocation" $ \cid -> do
            ic_update_settings' ic00 cid (#compute_allocation .== 101)
               >>= isReject [3,5]
        , simpleTestCase "Invalid memory allocation (2^49)" $ \cid -> do
            ic_update_settings' ic00 cid (#compute_allocation .== 2^(49::Int))
               >>= isReject [3,5]
        , simpleTestCase "Invalid memory allocation (2^70)" $ \cid -> do
            ic_update_settings' ic00 cid (#compute_allocation .== 2^(70::Int))
               >>= isReject [3,5]
        , simpleTestCase "Invalid freezing threshold (2^70)" $ \cid -> do
            ic_update_settings' ic00 cid (#freezing_threshold .== 2^(70::Int))
               >>= isReject [3,5]
        ]
    ]

  , testGroup "anonymous user"
    [ simpleTestCase "update, sender absent fails" $ \cid ->
      do envelopeFor anonymousUser $ rec
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "update"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= postCallCBOR cid >>= code4xx
    , simpleTestCase "query, sender absent fails" $ \cid ->
      do envelopeFor anonymousUser $ rec
          [ "request_type" =: GText "query"
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= postQueryCBOR cid >>= code4xx
    , simpleTestCase "update, sender explicit" $ \cid ->
      do awaitCall cid $ rec
          [ "request_type" =: GText "call"
          , "canister_id" =: GBlob cid
          , "sender" =: GBlob anonymousUser
          , "method_name" =: GText "update"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= isReply >>= is anonymousUser
    , simpleTestCase "query, sender explicit" $ \cid ->
      do queryCBOR cid $ rec
          [ "request_type" =: GText "query"
          , "canister_id" =: GBlob cid
          , "sender" =: GBlob anonymousUser
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData caller))
          ]
        >>= queryResponse >>= isReply >>= is anonymousUser
    ]

  , testGroup "state"
    [ simpleTestCase "set/get" $ \cid -> do
      call_ cid $ setGlobal "FOO" >>> reply
      query cid (replyData getGlobal) >>= is "FOO"

    , simpleTestCase "set/set/get" $ \cid -> do
      call_ cid $ setGlobal "FOO" >>> reply
      call_ cid $ setGlobal "BAR" >>> reply
      query cid (replyData getGlobal) >>= is "BAR"

    , simpleTestCase "resubmission" $ \cid -> do
      -- Submits the same request (same nonce) twice, checks that
      -- the IC does not act twice.
      -- (Using growing stable memory as non-idempotent action)
      callTwice' cid (ignore (stableGrow (int 1)) >>> reply) >>= isReply >>= is ""
      query cid (replyData (i2b stableSize)) >>= is "\1\0\0\0"
    ]

  , testGroup "API availability" $
    {-
    This section checks various API calls in various contexts, to see
    if they trap when they should
    This mirros the table in https://docs.dfinity.systems/public/#system-api-imports

    -}
    let
      {-
      Contexts

      A context is a function of type
         (String, Prog -> TestCase, Prog -> TestCase)
      building a test for does-not-trap or does-trap
      -}
      contexts = mconcat
        [ "I" =: twoContexts
          (reqResponse (\prog -> do
            cid <- create
            install' cid prog
          ))
          (reqResponse (\prog -> do
            cid <- install noop
            upgrade' cid prog
          ))
        , "G" =: reqResponse (\prog -> do
            cid <- install (onPreUpgrade (callback prog))
            upgrade' cid noop
          )
        , "U" =: twoContexts
          (reqResponse (\prog -> do
            cid <- install noop
            call' cid (prog >>> reply)
          ))
          (reqResponse (\prog -> do
            cid <- install noop
            call cid >=> isRelay $ inter_update cid defArgs{
              other_side = prog >>> reply
            }
          ))
        , "Q" =: twoContexts
          (reqResponse (\prog -> do
            cid <- install noop
            query' cid (prog >>> reply)
          ))
          (reqResponse (\prog -> do
            cid <- install noop
            call cid >=> isRelay $ inter_query cid defArgs{
              other_side = prog >>> reply
            }
          ))
        , "Ry" =: reqResponse (\prog -> do
            cid <- install noop
            call' cid $ inter_query cid defArgs{
              on_reply = prog >>> reply
            }
          )
        , "Rt" =: reqResponse (\prog -> do
            cid <- install noop
            call' cid $ inter_query cid defArgs{
              on_reject = prog >>> reply,
              other_side = trap "trap!"
            }
          )
        , "C" =: boolTest (\prog -> do
            cid <- install noop
            call' cid >=> isReject [5] $ inter_query cid defArgs
              { other_side = reply
              , on_reply = trap "Trapping in on_reply"
              , on_cleanup = Just $ prog >>> setGlobal "Did not trap"
              }
            g <- query cid $ replyData getGlobal
            return (g == "Did not trap")
          )
        , "F" =: httpResponse (\prog -> do
            cid <- install (onInspectMessage (callback (prog >>> acceptMessage)))
            call'' cid reply
          )
        ]

      -- context builder helpers
      httpResponse act = (act >=> is2xx >=> void . isReply, act >=> isErrOrReject [5])
      reqResponse act = (act >=> void . isReply, act >=> isReject [5])
      boolTest act = (act >=> is True, act >=> is False)
      twoContexts (aNT1, aT1) (aNT2, aT2) = (\p -> aNT1 p >> aNT2 p,\p -> aT1 p >> aT2 p)

      -- assembling it all
      t name trapping prog
        | Just n <- find (not . (`HM.member` contexts)) s
        = error $ "Undefined context " ++ T.unpack n
        | otherwise =
        [ if cname `S.member` s
          then testCase (name ++ " works in " ++ T.unpack cname) $ actNT prog
          else testCase (name ++ " traps in " ++ T.unpack cname) $ actTrap prog
        | (cname, (actNT, actTrap)) <- HM.toList contexts
        ]
        where s = S.fromList (T.words trapping)

      star = "I G U Q Ry Rt C F"
      never = ""

    in concat
    [ t "msg_arg_data"                 "I U Q Ry F"  $ ignore argData
    , t "msg_caller"                   "I G U Q F"   $ ignore caller
    , t "msg_reject_code"              "Ry Rt"       $ ignore reject_code
    , t "msg_reject_msg"               "Rt"          $ ignore reject_msg
    , t "msg_reply_data_append"        "U Q Ry Rt"   $ replyDataAppend "Hey!"
    , t "msg_reply_data_append (\"\")" "U Q Ry Rt"   $ replyDataAppend ""
    , t "msg_reply"                    never           reply -- due to double reply
    , t "msg_reject"                   never         $ reject "rejecting" -- due to double reply
    , t "msg_cycles_available"         "U Rt Ry"     $ ignore getAvailableCycles
    , t "msg_cycles_refunded"          "Rt Ry"       $ ignore getRefund
    , t "msg_cycles_accept"            "U Rt Ry"     $ ignore (acceptCycles (int64 0))
    , t "canister_self"                star          $ ignore self
    , t "canister_cycle_balance"       star          $ ignore getBalance
    , t "call_new…call_perform"        "U Rt Ry"     $
        callNew "foo" "bar" "baz" "quux" >>>
        callDataAppend "foo" >>>
        callCyclesAdd (int64 0) >>>
        callPerform
    , t "call_set_cleanup"             never         $ callOnCleanup (callback noop)
    , t "call_data_append"             never         $ callDataAppend "foo"
    , t "call_cycles_add"              never         $ callCyclesAdd (int64 0)
    , t "call_perform"                 never           callPerform
    , t "stable_size"                  star          $ ignore stableSize
    , t "stable_grow"                  star          $ ignore $ stableGrow (int 1)
    , t "stable_write"                 star          $ stableWrite (int 0) ""
    , t "stable_read"                  star          $ ignore $ stableRead (int 0) (int 0)
    , t "certified_data_set"           "I G U Ry Rt" $ setCertifiedData "foo"
    , t "data_certificate_present"     star          $ ignore getCertificatePresent
    , t "msg_method_name"              "F"           $ ignore methodName
    , t "accept_message"               never           acceptMessage -- due to double accept
    , t "time"                         star          $ ignore getTime
    , t "debug_print"                  star          $ debugPrint "hello"
    , t "trap"                         never         $ trap "this better traps"
    ]

  , simpleTestCase "self" $ \cid ->
    query cid (replyData self) >>= is cid

  , testGroup "wrong url path"
    [ simpleTestCase "call request to query" $ \cid -> do
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postQueryCBOR cid >>= code4xx

    , simpleTestCase "query request to call" $ \cid -> do
      let req = rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postCallCBOR cid >>= code4xx

    , simpleTestCase "query request to read_state" $ \cid -> do
      let req = rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postReadStateCBOR cid >>= code4xx

    , simpleTestCase "read_state request to query" $ \cid -> do
      addNonceExpiryEnv readStateEmpty >>= postQueryCBOR cid >>= code4xx
    ]

  , testGroup "wrong effective canister id"
    [ simpleTestCase "in call" $ \cid1 -> do
      cid2 <- create
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid1
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postCallCBOR cid2 >>= code4xx

    , simpleTestCase "in query" $ \cid1 -> do
      cid2 <- create
      let req = rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid1
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postQueryCBOR cid2 >>= code4xx

    -- read_state tested in read_state group
    --
    , simpleTestCase "in mangement call" $ \cid1 -> do
      cid2 <- create
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob ""
            , "method_name" =: GText "canister_status"
            , "arg" =: GBlob (Candid.encode (#canister_id .== Principal cid1))
            ]
      addNonceExpiryEnv req >>= postCallCBOR cid2 >>= code4xx

    , simpleTestCase "non-existing (and likely invalid)" $ \cid1 -> do
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid1
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      addNonceExpiryEnv req >>= postCallCBOR "foobar" >>= code4xx

    , simpleTestCase "invalid textual represenation" $ \cid1 -> do
      let req = rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob defaultUser
            , "canister_id" =: GBlob cid1
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      let path = "/api/v2/canister/" ++ filter (/= '-') (textual cid1) ++ "/call"
      addNonceExpiryEnv req >>= postCBOR path >>= code4xx
    ]

  , testGroup "inter-canister calls"
    [ testGroup "builder interface"
      [ testGroup "traps without call_new"
        [ simpleTestCase "call_data_append" $ \cid ->
          call' cid (callDataAppend "Foo" >>> reply) >>= isReject [5]
        , simpleTestCase "call_on_cleanup" $ \cid ->
          call' cid (callOnCleanup (callback noop) >>> reply) >>= isReject [5]
        , simpleTestCase "call_cycles_add" $ \cid ->
          call' cid (callCyclesAdd (int64 0) >>> reply) >>= isReject [5]
        , simpleTestCase "call_perform" $ \cid ->
          call' cid (callPerform >>> reply) >>= isReject [5]
        ]
      , simpleTestCase "call_new clears pending call" $ \cid -> do
        do call cid $
            callNew "foo" "bar" "baz" "quux" >>>
            callDataAppend "hey" >>>
            inter_query cid defArgs
          >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid)
      , simpleTestCase "call_data_append really appends" $ \cid -> do
        do call cid $
            callNew (bytes cid) (bytes "query")
                    (callback relayReply) (callback relayReject) >>>
            callDataAppend (bytes (BS.take 3 (run defaultOtherSide))) >>>
            callDataAppend (bytes (BS.drop 3 (run defaultOtherSide))) >>>
            callPerform
         >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid)
      , simpleTestCase "call_on_cleanup traps if called twice" $ \cid ->
        do call' cid $
            callNew (bytes cid) (bytes "query")
                    (callback relayReply) (callback relayReject) >>>
            callOnCleanup (callback noop) >>>
            callOnCleanup (callback noop) >>>
            reply
         >>= isReject [5]
      ]

    , simpleTestCase "to nonexistant canister" $ \cid ->
      call cid (inter_call "foo" "bar" defArgs) >>= isRelay >>= isReject [3]

    , simpleTestCase "to nonexistant method" $ \cid ->
      call cid (inter_call cid "bar" defArgs) >>= isRelay >>= isReject [3]

    , simpleTestCase "Call from query method traps (in update call)" $ \cid ->
      callToQuery'' cid (inter_query cid defArgs) >>= is2xx >>= isReject [5]

    , simpleTestCase "Call from query method traps (in query call)" $ \cid ->
      query' cid (inter_query cid defArgs) >>= isReject [5]

    , simpleTestCase "Call from query method traps (in inter-canister-call)" $ \cid ->
      do call cid $
          inter_call cid "query" defArgs {
            other_side = inter_query cid defArgs
          }
        >>= isRelay >>= isReject [5]

    , simpleTestCase "Self-call (to update)" $ \cid ->
      call cid (inter_update cid defArgs)
        >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid)

    , simpleTestCase "Self-call (to query)" $ \cid -> do
      call cid (inter_query cid defArgs)
        >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid)

    , simpleTestCase "update commits" $ \cid -> do
      do call cid $
          setGlobal "FOO" >>>
          inter_update cid defArgs{ other_side = setGlobal "BAR" >>> reply }
       >>= isRelay >>= isReply >>= is ""

      query cid (replyData getGlobal) >>= is "BAR"

    , simpleTestCase "query does not commit" $ \cid -> do
      do call cid $
          setGlobal "FOO" >>>
          inter_query cid defArgs{ other_side = setGlobal "BAR" >>> reply }
       >>= isRelay >>= isReply >>= is ""

      do query cid $ replyData getGlobal
        >>= is "FOO"

    , simpleTestCase "query no response" $ \cid ->
      do call cid $ inter_query cid defArgs{ other_side = noop }
        >>= isRelay >>= isReject [5]

    , simpleTestCase "query double reply" $ \cid ->
      do call cid $ inter_query cid defArgs{ other_side = reply >>> reply }
        >>= isRelay >>= isReject [5]

    , simpleTestCase "Reject code is 0 in reply" $ \cid ->
      do call cid $ inter_query cid defArgs{ on_reply = replyData (i2b reject_code) }
        >>= asWord32 >>= is 0

    , simpleTestCase "Second reply in callback" $ \cid -> do
      do call cid $
          setGlobal "FOO" >>>
          replyData "First reply" >>>
          inter_query cid defArgs{
            on_reply = setGlobal "BAR" >>> replyData "Second reply",
            on_reject = setGlobal "BAZ" >>> relayReject
          }
        >>= is "First reply"

      -- now check that the callback trapped and did not actual change the global
      -- to make this test reliabe, stop and start the canister, this will
      -- ensure all outstanding callbacks are handled
      barrier [cid]

      query cid (replyData getGlobal) >>= is "FOO"

    , simpleTestCase "partial reply" $ \cid ->
      do call cid $
          replyDataAppend "FOO" >>>
          inter_query cid defArgs{ on_reply = replyDataAppend "BAR" >>> reply }
        >>= is "BAR" -- check that the FOO is silently dropped

    , simpleTestCase "cleanup not executed when reply callback does not trap" $ \cid -> do
      call_ cid $ inter_query cid defArgs
        { on_reply = reply
        , on_cleanup = Just (setGlobal "BAD")
        }
      query cid (replyData getGlobal) >>= is ""

    , simpleTestCase "cleanup not executed when reject callback does not trap" $ \cid -> do
      call_ cid $ inter_query cid defArgs
        { other_side = reject "meh"
        , on_reject = reply
        , on_cleanup = Just (setGlobal "BAD")
        }
      query cid (replyData getGlobal) >>= is ""

    , testGroup "two callbacks"
      [ simpleTestCase "reply after trap" $ \cid ->
        do call cid $
            inter_query cid defArgs{ on_reply = trap "first callback traps" } >>>
            inter_query cid defArgs{ on_reply = replyData "good" }
          >>= is "good"

      , simpleTestCase "trap after reply" $ \cid ->
        do call cid $
            inter_query cid defArgs{ on_reply = replyData "good" } >>>
            inter_query cid defArgs{ on_reply = trap "second callback traps" }
         >>= is "good"

      , simpleTestCase "both trap" $ \cid ->
        do call' cid $
            inter_query cid defArgs{ on_reply = trap "first callback traps" } >>>
            inter_query cid defArgs{ on_reply = trap "second callback traps" }
          >>= isReject [5]
      ]

    , simpleTestCase "Call to other canister (to update)" $ \cid -> do
      cid2 <- install noop
      do call cid $ inter_update cid2 defArgs
        >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid2)

    , simpleTestCase "Call to other canister (to query)" $ \cid -> do
      cid2 <- install noop
      do call cid $ inter_query cid2 defArgs
        >>= isRelay >>= isReply >>= is ("Hello " <> cid <> " this is " <> cid2)
    ]

  , testCaseSteps "stable memory" $ \step -> do
    cid <- install noop

    step "Stable mem size is zero"
    query cid (replyData (i2b stableSize)) >>= is "\x0\x0\x0\x0"

    step "Writing stable memory (failing)"
    call' cid (stableWrite (int 0) "FOO") >>= isReject [5]
    step "Set stable mem (failing, query)"
    query' cid (stableWrite (int 0) "FOO") >>= isReject [5]

    step "Growing stable memory"
    call cid (replyData (i2b (stableGrow (int 1)))) >>= is "\x0\x0\x0\x0"

    step "Growing stable memory again"
    call cid (replyData (i2b (stableGrow (int 1)))) >>= is "\x1\x0\x0\x0"

    step "Growing stable memory in query"
    query cid (replyData (i2b (stableGrow (int 1)))) >>= is "\x2\x0\x0\x0"

    step "Stable mem size is two"
    query cid (replyData (i2b stableSize)) >>= is "\x2\x0\x0\x0"

    step "Writing stable memory"
    call_ cid $ stableWrite (int 0) "FOO" >>> reply

    step "Writing stable memory (query)"
    query_ cid $ stableWrite (int 0) "BAR" >>> reply

    step "Reading stable memory"
    call cid (replyData (stableRead (int 0) (int 3))) >>= is "FOO"

  , testGroup "time" $
    let getTimeTwice = cat (i64tob getTime) (i64tob getTime) in
    [ simpleTestCase "in query" $ \cid ->
      query cid (replyData getTimeTwice) >>= as2Word64 >>= bothSame
    , simpleTestCase "in update" $ \cid ->
      query cid (replyData getTimeTwice) >>= as2Word64 >>= bothSame
    , testCase "in install" $ do
      cid <- install $ setGlobal (getTimeTwice)
      query cid (replyData getGlobal) >>= as2Word64 >>= bothSame
    , testCase "in pre_upgrade" $ do
      cid <- install $
        ignore (stableGrow (int 1)) >>>
        onPreUpgrade (callback $ stableWrite (int 0) (getTimeTwice))
      upgrade cid noop
      query cid (replyData (stableRead (int 0) (int (2*8)))) >>= as2Word64 >>= bothSame
    , simpleTestCase "in post_upgrade" $ \cid -> do
      upgrade cid $ setGlobal (getTimeTwice)
      query cid (replyData getGlobal) >>= as2Word64 >>= bothSame
    ]

  , testGroup "upgrades" $
    let installForUpgrade on_pre_upgrade = install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "BAR______" >>>
            onPreUpgrade (callback on_pre_upgrade)

        checkNoUpgrade cid = do
          query cid (replyData getGlobal) >>= is "FOO"
          query cid (replyData (stableRead (int 0) (int 9))) >>= is "BAR______"
    in
    [ testCase "succeeding" $ do
      -- check that the pre-upgrade hook has access to the old state
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade cid $ stableWrite (int 6) (stableRead (int 0) (int 3))

      query cid (replyData getGlobal) >>= is ""
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "BARFOOBAR"

    , testCase "trapping in pre-upgrade" $ do
      cid <- installForUpgrade $ trap "trap in pre-upgrade"
      checkNoUpgrade cid

      upgrade' cid noop >>= isReject [5]
      checkNoUpgrade cid

    , testCase "trapping in pre-upgrade (by calling)" $ do
      cid <- installForUpgrade $ trap "trap in pre-upgrade"
      call_ cid $
        reply >>>
        onPreUpgrade (callback (
            inter_query cid defArgs { other_side = noop }
        ))
      checkNoUpgrade cid

      upgrade' cid noop >>= isReject [5]
      checkNoUpgrade cid

    , testCase "trapping in pre-upgrade (by accessing arg)" $ do
      cid <- installForUpgrade $ ignore argData
      checkNoUpgrade cid

      upgrade' cid noop >>= isReject [5]
      checkNoUpgrade cid

    , testCase "trapping in post-upgrade" $ do
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      upgrade' cid (trap "trap in post-upgrade") >>= isReject [5]
      checkNoUpgrade cid

    , testCase "trapping in post-upgrade (by calling)" $ do
      cid <- installForUpgrade $ stableWrite (int 3) getGlobal
      checkNoUpgrade cid

      do upgrade' cid $ inter_query cid defArgs{ other_side = noop }
        >>= isReject [5]
      checkNoUpgrade cid
    ]

  , testGroup "reinstall"
    [ testCase "succeeding" $ do
      cid <- install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "FOO______"
      query cid (replyData getGlobal) >>= is "FOO"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "FOO______"
      query cid (replyData (i2b stableSize)) >>= asWord32 >>= is 1

      reinstall cid $
        setGlobal "BAR" >>>
        ignore (stableGrow (int 2)) >>>
        stableWrite (int 0) "BAR______"

      query cid (replyData getGlobal) >>= is "BAR"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "BAR______"
      query cid (replyData (i2b stableSize)) >>= asWord32 >>= is 2

      reinstall cid noop

      query cid (replyData getGlobal) >>= is ""
      query cid (replyData (i2b stableSize)) >>= asWord32 >>= is 0

    , testCase "trapping" $ do
      cid <- install $
            setGlobal "FOO" >>>
            ignore (stableGrow (int 1)) >>>
            stableWrite (int 0) "FOO______"
      query cid (replyData getGlobal) >>= is "FOO"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "FOO______"
      query cid (replyData (i2b stableSize)) >>= is "\1\0\0\0"

      reinstall' cid (trap "Trapping the reinstallation") >>= isReject [5]

      query cid (replyData getGlobal) >>= is "FOO"
      query cid (replyData (stableRead (int 0) (int 9))) >>= is "FOO______"
      query cid (replyData (i2b stableSize)) >>= is "\1\0\0\0"
    ]

  , testGroup "uninstall" $
    let inter_management method_name cid on_reply =
          callNew "" method_name (callback on_reply) (callback relayReject) >>>
          callDataAppend (bytes (Candid.encode (#canister_id .== Principal cid))) >>>
          callPerform

        inter_install_code cid wasm_module on_reply =
          callNew "" "install_code" (callback on_reply) (callback relayReject) >>>
          callDataAppend (bytes (Candid.encode (empty
            .+ #canister_id .== Principal cid
            .+ #mode .== (enum #install :: InstallMode)
            .+ #wasm_module .== wasm_module
            .+ #arg .== run (setGlobal "NONE")
          ))) >>>
          callPerform
    in
    [ testCase "uninstall empty canister" $ do
      cid <- create
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running
      cs .! #settings .! #controller @?= Principal defaultUser
      cs .! #module_hash @?= Nothing
      ic_uninstall ic00 cid
      cs <- ic_canister_status ic00 cid
      cs .! #status @?= enum #running
      cs .! #settings .! #controller @?= Principal defaultUser
      cs .! #module_hash @?= Nothing

    , testCase "uninstall as wrong user" $ do
      cid <- create
      ic_uninstall'' otherUser cid >>= isErrOrReject [3,5]

    , testCase "uninstall and reinstall wipes state" $ do
      cid <- install (setGlobal "FOO")
      ic_uninstall ic00 cid
      universal_wasm <- getTestWasm "universal_canister"
      ic_install ic00 (enum #install) cid universal_wasm (run (setGlobal "BAR"))
      query cid (replyData getGlobal) >>= is "BAR"

    , testCase "uninstall and reinstall wipes stable memory" $ do
      cid <- install (ignore (stableGrow (int 1)) >>> stableWrite (int 0) "FOO")
      ic_uninstall ic00 cid
      universal_wasm <- getTestWasm "universal_canister"
      ic_install ic00 (enum #install) cid universal_wasm (run (setGlobal "BAR"))
      query cid (replyData (i2b stableSize)) >>= asWord32 >>= is 0
      do query cid $
          ignore (stableGrow (int 1)) >>>
          replyData (stableRead (int 0) (int 3))
       >>= is "\0\0\0"
      do call cid $
          ignore (stableGrow (int 1)) >>>
          replyData (stableRead (int 0) (int 3))
       >>= is "\0\0\0"

    , testCase "uninstall and reinstall wipes certified data" $ do
      cid <- install $ setCertifiedData "FOO"
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
      ic_uninstall ic00 cid
      universal_wasm <- getTestWasm "universal_canister"
      ic_install ic00 (enum #install) cid universal_wasm (run noop)
      query cid (replyData getCertificate) >>= extractCertData cid >>= is ""

    , simpleTestCase "uninstalled rejects calls" $ \cid -> do
      call cid (replyData "Hi") >>= is "Hi"
      query cid (replyData "Hi") >>= is "Hi"
      ic_uninstall ic00 cid
      -- should be http error, due to inspection
      call'' cid (replyData "Hi") >>= isErrOrReject []
      query' cid (replyData "Hi") >>= isReject [3]

    , testCase "open call contexts are rejected" $ do
      cid1 <- install noop
      cid2 <- install noop
      ic_set_controller ic00 cid1 cid2
      -- Step A: 2 calls 1
      -- Step B: 1 calls 2. Now 2 is waiting on a call from 1
      -- Step C: 2 uninstalls 1
      -- Step D: 2 replies "FOO"
      -- What should happen: The system rejects the call from step A
      -- What should not happen: The reply "FOO" makes it to canister 2
      -- (This is not a great test, because even if the call context is
      -- not rejected by the system during uninstallation it will somehow be rejected
      -- later when the callback is delivered to the empty canister. Maybe the reject
      -- code will catch it.)
      do call cid2 $ inter_update cid1 defArgs
          { other_side =
            inter_update cid2 defArgs
              { other_side =
                  inter_management "uninstall_code" cid1 $
                  replyData "FOO"
              }
          }
       >>= isRelay >>= isReject [4]

    , testCase "deleted call contexts do not prevent stopping" $ do
      -- Similar to above, but 2, after uninstalling, imediatelly
      -- stops and deletes 1. This can only work if the call
      -- contexts at 1 are indeed deleted
      cid1 <- install noop
      cid2 <- install noop
      ic_set_controller ic00 cid1 cid2
      do call cid2 $ inter_update cid1 defArgs
          { other_side =
            inter_update cid2 defArgs
              { other_side =
                  inter_management "uninstall_code" cid1 $
                  inter_management "stop_canister" cid1 $
                  inter_management "delete_canister" cid1 $
                  replyData "FOO"
              }
          }
       >>= isRelay >>= isReject [4]

      -- wait for cid2 to finish its stuff
      barrier [cid2]

      -- check that cid1 is deleted
      query' cid1 reply >>= isReject [3]

    , testCase "deleted call contexts are not delivered" $ do
      -- This is a very tricky one:
      -- Like above, but before replying, 2 installs code,
      -- calls into 1, so that 1 can call back to 2. This
      -- creates a new callback at 1, presumably with the same
      -- internal `env` as the one from step B.
      -- 2 rejects the new call, and replies to the original one.
      -- Only the on_reject callback at 1 should be called, not the on_reply
      -- We observe that using the “global”
      cid1 <- install noop
      cid2 <- install noop
      universal_wasm <- getTestWasm "universal_canister"
      ic_set_controller ic00 cid1 cid2
      do call cid2 $ inter_update cid1 defArgs
          { other_side =
            inter_update cid2 defArgs
              { other_side =
                  inter_management "uninstall_code" cid1 $
                  inter_install_code cid1 universal_wasm $
                  inter_update cid1 defArgs {
                    other_side =
                      inter_update cid2 defArgs
                      { other_side = reject "FOO"
                      , on_reply = setGlobal "REPLY" >>> reply
                      , on_reject = setGlobal "REJECT" >>> reply
                      }
                  }
              }
          }
       >>= isRelay >>= isReject [4]

      -- wait for cid2 to finish its stuff
      barrier [cid2]

      -- check cid1’s global
      query cid1 (replyData getGlobal) >>= is "REJECT"
    ]

  , testGroup "debug facilities"
    [ simpleTestCase "Using debug_print" $ \cid ->
      call_ cid (debugPrint "ic-ref-test print" >>> reply)
    , simpleTestCase "Using debug_print (query)" $ \cid ->
      query_ cid $ debugPrint "ic-ref-test print" >>> reply
    , simpleTestCase "Using debug_print with invalid bounds" $ \cid ->
      query_ cid $ badPrint >>> reply
    , simpleTestCase "Explicit trap" $ \cid ->
      call' cid (trap "trapping") >>= isReject [5]
    , simpleTestCase "Explicit trap (query)" $ \cid -> do
      query' cid (trap "trapping") >>= isReject [5]
    ]

  , testCase "caller (in init)" $ do
    cid <- install $ setGlobal caller
    query cid (replyData getGlobal) >>= is defaultUser

  , testCase "self (in init)" $ do
    cid <- install $ setGlobal self
    query cid (replyData getGlobal) >>= is cid

  , testGroup "trapping in init" $
    let
      failInInit pgm = do
        cid <- create
        install' cid pgm >>= isReject [5]
        -- canister does not exist
        query' cid noop >>= isReject [3]
    in
    [ testCase "explicit trap" $ failInInit $ trap "trapping in install"
    , testCase "call" $ failInInit $ inter_query "dummy" defArgs
    , testCase "reply" $ failInInit reply
    , testCase "reject" $ failInInit $ reject "rejecting in init"
    ]

  , testGroup "query"
    [ testGroup "required fields" $ do
        -- TODO: Begin with a succeeding request to a real canister, to rule
        -- out other causes of failure than missing fields
        omitFields queryToNonExistant $ \req -> do
          cid <- create
          addExpiry req >>= envelope defaultSK >>= postQueryCBOR cid >>= code4xx

    , simpleTestCase "non-existing (deleted) canister" $ \cid -> do
        ic_stop_canister ic00 cid
        ic_delete_canister ic00 cid
        query' cid reply >>= isReject [3]

    , simpleTestCase "does not commit" $ \cid -> do
        call_ cid (setGlobal "FOO" >>> reply)
        query cid (setGlobal "BAR" >>> replyData getGlobal) >>= is "BAR"
        query cid (replyData getGlobal) >>= is "FOO"
    ]

  , testGroup "read state" $
    let ensure_request_exists cid user = do
          req <- addNonce >=> addExpiry $ rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob user
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run (replyData "\xff\xff"))
            ]
          awaitCall cid req >>= isReply >>= is "\xff\xff"

          -- check that the request is there
          getRequestStatus user cid (requestId req) >>= is (Responded (Reply "\xff\xff"))

          return (requestId req)
    in
    [ testGroup "required fields" $
        omitFields readStateEmpty $ \req -> do
          cid <- create
          addExpiry req >>= envelope defaultSK >>= postReadStateCBOR cid >>= code4xx

    , simpleTestCase "certificate validates" $ \cid -> do
        cert <- getStateCert defaultUser cid []
        validateStateCert cert

    , testCaseSteps "time is present" $ \step -> do
        cid <- create
        cert <- getStateCert defaultUser cid []
        time <- certValue @Natural cert ["time"]
        step $ "Reported time: " ++ show time

    , testCase "time can be asked for" $ do
        cid <- create
        cert <- getStateCert defaultUser cid [["time"]]
        void $ certValue @Natural cert ["time"]

    , testCase "controller of empty canister" $ do
        cid <- create
        cert <- getStateCert defaultUser cid [["canister", cid, "controller"]]
        certValue @Blob cert ["canister", cid, "controller"] >>= is defaultUser

    , testCase "module_hash of empty canister" $ do
        cid <- create
        cert <- getStateCert defaultUser cid [["canister", cid, "module_hash"]]
        lookupPath (cert_tree cert) ["canister", cid, "module_hash"] @?= Absent

    , testCase "controller of installed canister" $ do
        cid <- install noop
        -- also vary user, just for good measure
        cert <- getStateCert anonymousUser cid [["canister", cid, "controller"]]
        certValue @Blob cert ["canister", cid, "controller"] >>= is defaultUser

    , testCase "module_hash of empty canister" $ do
        cid <- install noop
        universal_wasm <- getTestWasm "universal_canister"
        cert <- getStateCert anonymousUser cid [["canister", cid, "module_hash"]]
        certValue @Blob cert ["canister", cid, "module_hash"] >>= is (sha256 universal_wasm)

    , testGroup "non-existence proofs for non-existing request id"
        [ simpleTestCase ("rid \"" ++ shorten 8 (asHex rid) ++ "\"") $ \cid -> do
            cert <- getStateCert defaultUser cid [["request_status", rid]]
            certValueAbsent cert ["request_status", rid, "status"]
        | rid <- [ "", BS.replicate 32 0, BS.replicate 32 8, BS.replicate 32 255 ]
        ]

    , simpleTestCase "can ask for portion of request status " $ \cid -> do
        rid <- ensure_request_exists cid defaultUser
        cert <- getStateCert defaultUser cid
          [["request_status", rid, "status"], ["request_status", rid, "reply"]]
        void $ certValue @T.Text cert ["request_status", rid, "status"]
        void $ certValue @Blob cert ["request_status", rid, "reply"]

    , simpleTestCase "access denied for other users request" $ \cid -> do
        rid <- ensure_request_exists cid defaultUser
        getStateCert' otherUser cid [["request_status", rid]] >>= code4xx

    , simpleTestCase "reading two statuses to same canister in one go" $ \cid -> do
        rid1 <- ensure_request_exists cid defaultUser
        rid2 <- ensure_request_exists cid defaultUser
        cert <- getStateCert defaultUser cid [["request_status", rid1], ["request_status", rid2]]
        void $ certValue @T.Text cert ["request_status", rid1, "status"]
        void $ certValue @T.Text cert ["request_status", rid2, "status"]

    , simpleTestCase "access denied for other users request (mixed request)" $ \cid -> do
        rid1 <- ensure_request_exists cid defaultUser
        rid2 <- ensure_request_exists cid otherUser
        getStateCert' defaultUser cid [["request_status", rid1], ["request_status", rid2]] >>= code4xx

    , simpleTestCase "access denied two status to different canisters" $ \cid -> do
        cid2 <- install noop
        rid1 <- ensure_request_exists cid defaultUser
        rid2 <- ensure_request_exists cid2 defaultUser
        getStateCert' defaultUser cid [["request_status", rid1], ["request_status", rid2]] >>= code4xx

    , simpleTestCase "access denied for bogus path" $ \cid -> do
        getStateCert' otherUser cid [["hello", "world"]] >>= code4xx

    , simpleTestCase "access denied for fetching full state tree" $ \cid -> do
        getStateCert' otherUser cid [[]] >>= code4xx
    ]

  , testGroup "certified variables"
    [ simpleTestCase "initially empty" $ \cid -> do
      query cid (replyData getCertificate) >>= extractCertData cid >>= is ""
    , simpleTestCase "validates" $ \cid -> do
      query cid (replyData getCertificate)
        >>= decodeCert' >>= validateStateCert
    , simpleTestCase "present in query method (query call)" $ \cid -> do
      query cid (replyData (i2b getCertificatePresent))
        >>= is "\1\0\0\0"
    , simpleTestCase "not present in query method (update call)" $ \cid -> do
      callToQuery'' cid (replyData (i2b getCertificatePresent))
        >>= is2xx >>= isReply >>= is "\0\0\0\0"
    , simpleTestCase "not present in query method (inter-canister call)" $ \cid -> do
      do call cid $
          inter_call cid "query" defArgs {
            other_side = replyData (i2b getCertificatePresent)
          }
        >>= isRelay >>= isReply >>= is "\0\0\0\0"
    , simpleTestCase "not present in update method" $ \cid -> do
      call cid (replyData (i2b getCertificatePresent))
        >>= is "\0\0\0\0"

    , simpleTestCase "set and get" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , simpleTestCase "set twice" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> setCertifiedData "BAR" >>> reply
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "BAR"
    , simpleTestCase "set then trap" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      call' cid (setCertifiedData "BAR" >>> trap "Trapped") >>= isReject [5]
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , simpleTestCase "too large traps, old value retained" $ \cid -> do
      call_ cid $ setCertifiedData "FOO" >>> reply
      call' cid (setCertifiedData (bytes (BS.replicate 33 0x42)) >>> reply)
        >>= isReject [5]
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , testCase "set in init" $ do
      cid <- install $ setCertifiedData "FOO"
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , testCase "set in pre-upgrade" $ do
      cid <- install $ onPreUpgrade (callback $ setCertifiedData "FOO")
      upgrade cid noop
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    , simpleTestCase "set in post-upgrade" $ \cid -> do
      upgrade cid $ setCertifiedData "FOO"
      query cid (replyData getCertificate) >>= extractCertData cid >>= is "FOO"
    ]

  , testGroup "cycles" $
    let replyBalance = replyData (i64tob getBalance)
        rememberBalance =
          ignore (stableGrow (int 1)) >>>
          stableWrite (int 0) (i64tob getBalance)
        recallBalance = replyData (stableRead (int 0) (int 8))
        acceptAll = ignore (acceptCycles getAvailableCycles)
        queryBalance cid = query cid replyBalance >>= asWord64

        -- At the time of writing, creating a canister needs at least 1T
        -- and the freezing limit is 5T
        -- (At some point, the max was 100T, but that is no longer the case)
        -- So lets try to stay away from these limits.
        -- The lowest denomination we deal with below is def_cycles`div`4
        def_cycles = 80_000_000_000_000 :: Word64

        -- The system burns cycles at unspecified rates. To cater for such behaviour,
        -- we make the assumption that no test burns more than the following epsilon.
        --
        -- The biggest fee we currenlty deal with is the system deducing 1T
        -- upon canister creation. So our epsilon needs to allow that and then
        -- some more.
        eps = 3_000_000_000_000 :: Integer

        isRoughly :: (HasCallStack, Show a, Num a, Integral a) => a -> a -> Assertion
        isRoughly exp act = assertBool
           (show act ++ " not near " ++ show exp)
           (abs (fromIntegral exp - fromIntegral act) < eps)

        create prog = do
          cid <- ic_provisional_create ic00 (Just (fromIntegral def_cycles)) empty
          installAt cid prog
          return cid
        create_via cid initial_cycles = do
          cid2 <- ic_create (ic00viaWithCycles cid initial_cycles) empty
          universal_wasm <- getTestWasm "universal_canister"
          ic_install (ic00via cid) (enum #install) cid2 universal_wasm (run noop)
          return cid2
    in
    [ testGroup "can use balance API" $
        let getBalanceTwice = join cat (i64tob getBalance)
            test = replyData getBalanceTwice
        in
        [ simpleTestCase "in query" $ \cid ->
          query cid test >>= as2Word64 >>= bothSame
        , simpleTestCase "in update" $ \cid ->
          call cid test >>= as2Word64 >>= bothSame
        , testCase "in init" $ do
          cid <- install (setGlobal getBalanceTwice)
          query cid (replyData getGlobal) >>= as2Word64 >>= bothSame
        , simpleTestCase "in callback" $ \cid ->
          call cid (inter_query cid defArgs{ on_reply = test }) >>= as2Word64 >>= bothSame
        ]
    , testGroup "can use available cycles API" $
        let getAvailableCyclesTwice = join cat (i64tob getAvailableCycles)
            test = replyData getAvailableCyclesTwice
        in
        [ simpleTestCase "in update" $ \cid ->
          call cid test >>= as2Word64 >>= bothSame
        , simpleTestCase "in callback" $ \cid ->
          call cid (inter_query cid defArgs{ on_reply = test }) >>= as2Word64 >>= bothSame
        ]
    , simpleTestCase "can accept zero cycles" $ \cid ->
        call cid (replyData (i64tob (acceptCycles (int64 0)))) >>= asWord64 >>= is 0
    , simpleTestCase "can accept more than available cycles" $ \cid ->
        call cid (replyData (i64tob (acceptCycles (int64 1)))) >>= asWord64 >>= is 0
    , simpleTestCase "cant accept absurd amount of cycles" $ \cid ->
        call cid (replyData (i64tob (acceptCycles (int64 maxBound)))) >>= asWord64 >>= is 0

    , testGroup "provisional_create_canister_with_cycles"
      [ testCase "balance as expected" $ do
        cid <- create noop
        queryBalance cid >>= isRoughly def_cycles

      , testCaseSteps "default (i.e. max) balance" $ \step -> do
        cid <- ic_provisional_create ic00 Nothing empty
        installAt cid noop
        cycles <- queryBalance cid
        step $ "Cycle balance now at " ++ show cycles

      , testCaseSteps "> 2^128 succeeds" $ \step -> do
        cid <- ic_provisional_create ic00 (Just (10 * 2^(128::Int))) empty
        installAt cid noop
        cycles <- queryBalance cid
        step $ "Cycle balance now at " ++ show cycles
      ]

    , testCase "cycles in canister_status" $ do
        cid <- create noop
        cs <- ic_canister_status ic00 cid
        isRoughly (fromIntegral def_cycles) (cs .! #cycles)

    , testGroup "cycle balance"
      [ testCase "install" $ do
        cid <- create rememberBalance
        query cid recallBalance >>= asWord64 >>= isRoughly def_cycles
      , testCase "update" $ do
        cid <- create noop
        call cid replyBalance >>= asWord64 >>= isRoughly def_cycles
      , testCase "query" $ do
        cid <- create noop
        query cid replyBalance >>= asWord64 >>= isRoughly def_cycles
      , testCase "in pre_upgrade" $ do
        cid <- create $ onPreUpgrade (callback rememberBalance)
        upgrade cid noop
        query cid recallBalance >>= asWord64 >>= isRoughly def_cycles
      , testCase "in post_upgrade" $ do
        cid <- create noop
        upgrade cid rememberBalance
        query cid recallBalance >>= asWord64 >>= isRoughly def_cycles
        queryBalance cid >>= isRoughly def_cycles
      ]
    , testCase "can send cycles" $ do
      cid1 <- create noop
      cid2 <- create noop
      do call cid1 $ inter_call cid2 "update" defArgs
          { other_side =
            replyDataAppend (i64tob getAvailableCycles) >>>
            acceptAll >>>
            reply
          , cycles = def_cycles `div` 4
          }
        >>= isRelay >>= isReply >>= asWord64 >>= isRoughly (def_cycles `div` 4)
      queryBalance cid1 >>= isRoughly (def_cycles - def_cycles `div` 4)
      queryBalance cid2 >>= isRoughly (def_cycles + def_cycles `div` 4)

    , testCase "sending more cycles than in balance traps" $ do
      cid <- create noop
      cycles <- queryBalance cid
      call' cid (inter_call cid cid defArgs { cycles = cycles + 1000_000 })
        >>= isReject [5]

    , testCase "relay cycles before accept traps" $ do
      cid1 <- create noop
      cid2 <- create noop
      cid3 <- create noop
      do call cid1 $ inter_call cid2 "update" defArgs
          { cycles = def_cycles `div` 2
          , other_side =
            inter_call cid3 "update" defArgs
              { other_side = acceptAll >>> reply
              , cycles = def_cycles + def_cycles `div` 4
              , on_reply = noop -- must not double reply
              } >>>
            acceptAll >>> reply
          , on_reply = trap "unexpected reply"
          , on_reject = replyData (i64tob getRefund)
          }
        >>= asWord64 >>= isRoughly (def_cycles `div` 2)
      queryBalance cid1 >>= isRoughly def_cycles
      queryBalance cid2 >>= isRoughly def_cycles
      queryBalance cid3 >>= isRoughly def_cycles
    , testCase "relay cycles after accept works" $ do
      cid1 <- create noop
      cid2 <- create noop
      cid3 <- create noop
      do call cid1 $ inter_call cid2 "update" defArgs
          { cycles = def_cycles `div` 2
          , other_side =
            acceptAll >>>
            inter_call cid3 "update" defArgs
              { other_side = acceptAll >>> reply
              , cycles = def_cycles + def_cycles `div` 4
              }
          , on_reply = replyData (i64tob getRefund)
          , on_reject = trap "unexpected reject"
          }
        >>= asWord64 >>= isRoughly 0
      queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
      queryBalance cid2 >>= isRoughly (def_cycles `div` 4)
      queryBalance cid3 >>= isRoughly (2*def_cycles + def_cycles `div` 4)
    , testCase "aborting call resets balance" $ do
      cid <- create noop
      (a,b) <- do
         call cid $
          callNew "Foo" "Bar" "baz" "quux" >>>
          callCyclesAdd (int64 (def_cycles `div` 2)) >>>
          replyDataAppend (i64tob getBalance) >>>
          callNew "Foo" "Bar" "baz" "quux" >>>
          replyDataAppend (i64tob getBalance) >>>
          reply
        >>= as2Word64
      isRoughly (def_cycles `div` 2) a
      isRoughly def_cycles b

    , testCase "partial refund" $ do
      cid1 <- create noop
      cid2 <- create noop
      do call cid1 $ inter_call cid2 "update" defArgs
          { cycles = def_cycles `div` 2
          , other_side = ignore (acceptCycles (int64 (def_cycles `div` 4))) >>> reply
          , on_reply = replyData (i64tob getRefund)
          , on_reject = trap "unexpected reject"
          }
        >>= asWord64 >>= isRoughly (def_cycles `div` 4)
      queryBalance cid1 >>= isRoughly (def_cycles - def_cycles `div` 4)
      queryBalance cid2 >>= isRoughly (def_cycles + def_cycles `div` 4)
    , testCase "cycles not in balance while in transit" $ do
      cid1 <- create noop
      do call cid1 $ inter_call cid1 "update" defArgs
          { cycles = def_cycles `div` 4
          , other_side = replyBalance
          , on_reject = trap "unexpected reject"
          }
        >>= isRelay >>= isReply >>= asWord64 >>= isRoughly (def_cycles - def_cycles `div` 4)
      queryBalance cid1 >>= isRoughly def_cycles
    , testCase "create and delete canister with cycles" $ do
      cid1 <- create noop
      cid2 <- create_via cid1 (def_cycles`div`2)
      queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
      queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
      ic_stop_canister (ic00via cid1) cid2
      -- We load some cycles on the deletion call, just to check that they are refunded
      ic_delete_canister (ic00viaWithCycles cid1 (def_cycles`div`4)) cid2
      queryBalance cid1 >>= isRoughly (def_cycles`div`2)

    , testGroup "deposit_cycles"
      [ testCase "as controller" $ do
        cid1 <- create noop
        cid2 <- create_via cid1 (def_cycles`div`2)
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
        ic_deposit_cycles (ic00viaWithCycles cid1 (def_cycles`div`4)) cid2
        queryBalance cid1 >>= isRoughly (def_cycles `div` 4)
        queryBalance cid2 >>= isRoughly (def_cycles - def_cycles `div` 4)
      , testCase "as other non-controlling canister" $ do
        cid1 <- create noop
        cid2 <- create_via cid1 (def_cycles`div`2)
        queryBalance cid1 >>= isRoughly (def_cycles `div` 2)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 2)
        ic_deposit_cycles (ic00viaWithCycles cid2 (def_cycles`div`4)) cid1
        queryBalance cid1 >>= isRoughly (def_cycles - def_cycles `div` 4)
        queryBalance cid2 >>= isRoughly (def_cycles `div` 4)
      , testCase "to non-existing canister" $ do
        cid1 <- create noop
        queryBalance cid1 >>= isRoughly def_cycles
        ic_deposit_cycles' (ic00viaWithCycles cid1 (def_cycles`div`4)) doesn'tExist
          >>= isReject [3,4,5]
        queryBalance cid1 >>= isRoughly def_cycles
      ]

    , testCase "two-step-refund" $ do
      cid1 <- create noop
      do call cid1 $ inter_call cid1 "update" defArgs
          { cycles = 10
          , other_side = inter_call cid1 "update" defArgs
              { cycles = 5
              , other_side = reply -- no accept
              , on_reply =
                    -- remember refund
                    replyDataAppend (i64tob getRefund) >>>
                    reply
              , on_reject = trap "unexpected reject"
              }
          , on_reply =
                -- remember the refund above and this refund
                replyDataAppend argData >>>
                replyDataAppend (i64tob getRefund) >>>
                reply
          , on_reject = trap "unexpected reject"
          }
        >>= as2Word64 >>= is (5,10)
      queryBalance cid1 >>= isRoughly def_cycles -- nothing lost?

    , testGroup "provisional top up"
      [ testCase "as user" $ do
        cid <- create noop
        queryBalance cid >>= isRoughly def_cycles
        ic_top_up ic00 cid (fromIntegral def_cycles)
        queryBalance cid >>= isRoughly (2 * def_cycles)
      , testCase "as self" $ do
        cid <- create noop
        queryBalance cid >>= isRoughly def_cycles
        ic_top_up (ic00via cid) cid (fromIntegral def_cycles)
        queryBalance cid >>= isRoughly (2 * def_cycles)
      , testCase "as other canister" $ do
        cid <- create noop
        cid2 <- create noop
        queryBalance cid >>= isRoughly def_cycles
        ic_top_up (ic00via cid2) cid (fromIntegral def_cycles)
        queryBalance cid >>= isRoughly (2 * def_cycles)
      , testCaseSteps "more than 2^128" $ \step -> do
        cid <- create noop
        ic_top_up ic00 cid (10 * 2^(128::Int))
        cycles <- queryBalance cid
        step $ "Cycle balance now at " ++ show cycles
      , testCase "nonexisting canister" $ do
        ic_top_up' ic00 doesn'tExist (fromIntegral def_cycles)
          >>= isReject [3,5]
      ]
    ]

  , testGroup "canister_inspect_message"
    [ testCase "empty canister" $ do
      cid <- create
      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

    , testCase "accept all" $ do
      cid <- install $ onInspectMessage $ callback acceptMessage
      call_ cid reply
      callToQuery'' cid reply >>= is2xx >>= isReply >>= is ""

    , testCase "no accept_message" $ do
      cid <- install $ onInspectMessage $ callback noop
      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []
      -- check that inter-canister calls still work
      cid2 <- install noop
      call cid2 (inter_update cid defArgs)
        >>= isRelay >>= isReply >>= is ("Hello " <> cid2 <> " this is " <> cid)

    , testCase "two calls to accept_message" $ do
      cid <- install $ onInspectMessage $ callback $ acceptMessage >>> acceptMessage
      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

    , testCase "trap" $ do
      cid <- install $ onInspectMessage $ callback $ trap "no no no"
      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

    , testCase "method_name correct" $ do
      cid <- install $ onInspectMessage $ callback $
        trapIfEq methodName "update" "no no no" >>> acceptMessage

      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= is2xx >>= isReply >>= is ""

    , testCase "caller correct" $ do
      cid <- install $ onInspectMessage $ callback $
        trapIfEq caller (bytes defaultUser) "no no no" >>> acceptMessage

      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

      awaitCall' cid (callRequestAs otherUser cid reply)
        >>= is2xx >>= isReply >>= is ""
      awaitCall' cid (callToQueryRequestAs otherUser cid reply)
        >>= is2xx >>= isReply >>= is ""

    , testCase "arg correct" $ do
      cid <- install $ onInspectMessage $ callback $
        trapIfEq argData (callback reply) "no no no" >>> acceptMessage

      call'' cid reply >>= isErrOrReject []
      callToQuery'' cid reply >>= isErrOrReject []

      call cid (replyData "foo") >>= is "foo"
      callToQuery'' cid (replyData "foo") >>= is2xx >>= isReply >>= is "foo"

    , testCase "management canister: raw_rand not accepted" $ do
      ic_raw_rand'' defaultUser >>= isErrOrReject []

    , simpleTestCase "management canister: deposit_cycles not accepted" $ \cid -> do
      ic_deposit_cycles'' defaultUser cid >>= isErrOrReject []

    , simpleTestCase "management canister: wrong sender not accepted" $ \cid -> do
      ic_canister_status'' otherUser cid >>= isErrOrReject []
    ]

  , testGroup "Delegation targets" $ let
      callReq cid = rec
        [ "request_type" =: GText "call"
        , "sender" =: GBlob defaultUser
        , "canister_id" =: GBlob cid
        , "method_name" =: GText "update"
        , "arg" =: GBlob (run reply)
        ]

      mgmtReq cid = rec
        [ "request_type" =: GText "call"
        , "sender" =: GBlob defaultUser
        , "canister_id" =: GBlob ""
        , "method_name" =: GText "canister_status"
        , "arg" =: GBlob (Candid.encode (#canister_id .== Principal cid))
        ]

      good cid req dels = do
        req <- addExpiry req
        let rid = requestId req
        -- sign request with delegations
        delegationEnv defaultSK dels req >>= postCallCBOR cid >>= code2xx
        -- wait for it
        void $ awaitStatus defaultUser cid rid >>= isReply
        -- also read status with delegation
        sreq <- addExpiry $ rec
          [ "request_type" =: GText "read_state"
          , "sender" =: GBlob defaultUser
          , "paths" =: GList [GList [GBlob "request_status", GBlob rid]]
          ]
        delegationEnv defaultSK dels sreq >>= postReadStateCBOR cid >>= void . code2xx

      badSubmit cid req dels = do
        req <- addExpiry req
        -- sign request with delegations (should fail)
        delegationEnv defaultSK dels req >>= postCallCBOR cid >>= code4xx

      badRead cid req dels = do
        req <- addExpiry req
        let rid = requestId req
        -- submit with plain signature
        envelope defaultSK req >>= postCallCBOR cid >>= code202
        -- wait for it
        void $ awaitStatus defaultUser cid rid >>= isReply
        -- also read status with delegation
        sreq <- addExpiry $ rec
          [ "request_type" =: GText "read_state"
          , "sender" =: GBlob defaultUser
          , "paths" =: GList [GList [GBlob "request_status", GBlob rid]]
          ]
        delegationEnv defaultSK dels sreq >>= postReadStateCBOR cid >>= void . code4xx

      goodTestCase name mkReq mkDels =
        simpleTestCase name $ \cid -> good cid (mkReq cid) (mkDels cid)

      badTestCase name mkReq mkDels = testGroup name
        [ simpleTestCase "in submit" $ \cid -> badSubmit cid (mkReq cid) (mkDels cid)
        , simpleTestCase "in read_state" $ \cid -> badRead cid (mkReq cid) (mkDels cid)
        ]

      withEd25519 = zip [createSecretKeyEd25519 (BS.singleton n) | n <- [0..]]
      withWebAuthn = zip [createSecretKeyWebAuthn (BS.singleton n) | n <- [0..]]

    in
    [ goodTestCase "one delegation, singleton target" callReq $ \cid ->
      withEd25519 [Just [cid]]
    , badTestCase "one delegation, wrong singleton target" callReq $ \_cid ->
      withEd25519 [Just [doesn'tExist]]
    , goodTestCase "one delegation, two targets" callReq $ \cid ->
      withEd25519 [Just [cid, doesn'tExist]]
    , goodTestCase "two delegations, two targets, webauthn" callReq $ \cid ->
      withWebAuthn [Just [cid, doesn'tExist], Just [cid, doesn'tExist]]
    , goodTestCase "one delegation, redundant targets" callReq $ \cid ->
      withEd25519 [Just [cid, cid, doesn'tExist]]
    , goodTestCase "two delegations, singletons" callReq $ \cid ->
      withEd25519 [Just [cid], Just [cid] ]
    , goodTestCase "two delegations, first restricted" callReq $ \cid ->
      withEd25519 [Just [cid], Nothing ]
    , goodTestCase "two delegations, second restricted" callReq $ \cid ->
      withEd25519 [Nothing, Just [cid]]
    , badTestCase "two delegations, empty intersection" callReq $ \cid ->
      withEd25519 [Just [cid], Just [doesn'tExist]]
    , badTestCase "two delegations, first empty target set" callReq $ \cid ->
      withEd25519 [Just [], Just [cid]]
    , badTestCase "two delegations, second empty target set" callReq $ \cid ->
      withEd25519 [Just [cid], Just []]
    , goodTestCase "management canister: correct target" mgmtReq $ \_cid ->
      withEd25519 [Just [""]]
    , badTestCase "management canister: empty target set" mgmtReq $ \_cid ->
      withEd25519 [Just []]
    , badTestCase "management canister: bogus target" mgmtReq $ \_cid ->
      withEd25519 [Just [doesn'tExist]]
    , badTestCase "management canister: bogus target (using target canister)" mgmtReq $ \cid ->
      withEd25519 [Just [cid]]
    ]

  , testGroup "Authentication schemes" $
    let ed25519SK2 = createSecretKeyEd25519 "more keys"
        ed25519SK3 = createSecretKeyEd25519 "yet more keys"
        ed25519SK4 = createSecretKeyEd25519 "even more keys"
        delEnv sks = delegationEnv otherSK (map (, Nothing) sks) -- no targets in these tests
    in flip foldMap
      [ ("Ed25519",            otherUser,      envelope otherSK)
      , ("ECDSA",              ecdsaUser,      envelope ecdsaSK)
      , ("secp256k1",          secp256k1User,  envelope secp256k1SK)
      , ("WebAuthn",           webAuthnUser,   envelope webAuthnSK)
      , ("empty delegations",  otherUser,      delEnv [])
      , ("same delegations",   otherUser,      delEnv [otherSK])
      , ("three delegations",  otherUser,      delEnv [ed25519SK2, ed25519SK3])
      , ("four delegations",   otherUser,      delEnv [ed25519SK2, ed25519SK3, ed25519SK4])
      , ("mixed delegations",  otherUser,      delEnv [defaultSK, webAuthnSK, ecdsaSK, secp256k1SK])
      ] $ \ (name, user, env) ->
    [ simpleTestCase (name ++ " in query") $ \cid -> do
      req <- addExpiry $ rec
            [ "request_type" =: GText "query"
            , "sender" =: GBlob user
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "query"
            , "arg" =: GBlob (run reply)
            ]
      signed_req <- env req
      postQueryCBOR cid signed_req >>= okCBOR >>= queryResponse >>= isReply >>= is ""

    , simpleTestCase (name ++ " in update") $ \cid -> do
      req <- addExpiry $ rec
            [ "request_type" =: GText "call"
            , "sender" =: GBlob user
            , "canister_id" =: GBlob cid
            , "method_name" =: GText "update"
            , "arg" =: GBlob (run reply)
            ]
      signed_req <- env req
      postCallCBOR cid signed_req >>= code2xx

      awaitStatus user cid (requestId req) >>= isReply >>= is ""
    ]

  , testGroup "signature checking" $
    [ ("with bad signature", return . badEnvelope, id)
    , ("with wrong key", envelope otherSK, id)
    , ("with empty domain separator", noDomainSepEnv defaultSK, id)
    , ("with no expiry", envelope defaultSK, noExpiryEnv)
    , ("with expiry in the past", envelope defaultSK, pastExpiryEnv)
    , ("with expiry in the future", envelope defaultSK, futureExpiryEnv)
    ] <&> \(name, env, mod_req) -> testGroup name
      [ simpleTestCase "in query" $ \cid -> do
        good_req <- addNonce >=> addExpiry $ rec
              [ "request_type" =: GText "query"
              , "sender" =: GBlob defaultUser
              , "canister_id" =: GBlob cid
              , "method_name" =: GText "query"
              , "arg" =: GBlob (run reply)
              ]
        queryCBOR cid good_req >>= queryResponse >>= isReply >>= is ""
        env (mod_req good_req) >>= postQueryCBOR cid >>= code4xx

      , simpleTestCase "in empty read state request" $ \cid -> do
          good_req <- addNonce >=> addExpiry $ readStateEmpty
          envelope defaultSK good_req >>= postReadStateCBOR cid >>= code2xx
          env (mod_req good_req) >>= postReadStateCBOR cid >>= code4xx

      , simpleTestCase "in call" $ \cid -> do
          good_req <- addNonce >=> addExpiry $ rec
                [ "request_type" =: GText "call"
                , "sender" =: GBlob defaultUser
                , "canister_id" =: GBlob cid
                , "method_name" =: GText "query"
                , "arg" =: GBlob (run reply)
                ]
          let req = mod_req good_req
          env req >>= postCallCBOR cid >>= code202_or_4xx

          -- Also check that the request was not created
          ingressDelay
          getRequestStatus defaultUser cid (requestId req) >>= is UnknownStatus

          -- check that with a valid signature, this would have worked
          awaitCall cid good_req >>= isReply >>= is ""
      ]

  , testGroup "Canister signatures" $
    let genId cid seed =
          DER.encode DER.CanisterSig $ CanisterSig.genPublicKey (EntityId cid) seed

        genSig cid seed msg = do
          -- Create the tree
          let tree = construct $
                SubTrees $ M.singleton "sig" $
                SubTrees $ M.singleton (sha256 seed) $
                SubTrees $ M.singleton (sha256 msg) $
                Value ""
          -- Store it as certified data
          call_ cid (setCertifiedData (bytes (reconstruct tree)) >>> reply)
          -- Get certificate
          cert <- query cid (replyData getCertificate) >>= decodeCert'
          -- double check it certifies
          validateStateCert cert
          certValue cert ["canister", cid, "certified_data"] >>= is (reconstruct tree)

          return $ CanisterSig.genSig cert tree

        exampleQuery cid userKey = addExpiry $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob (mkSelfAuthenticatingId userKey)
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData "It works!"))
          ]
        simpleEnv userKey sig req delegations = rec $
          [ "sender_pubkey" =: GBlob userKey
          , "sender_sig" =: GBlob sig
          , "content" =: req
          ] ++
          [ "sender_delegation" =: GList delegations | not (null delegations) ]
    in
    [ simpleTestCase "direct signature" $ \cid -> do
      let userKey = genId cid "Hello!"
      req <- exampleQuery cid userKey
      sig <- genSig cid "Hello!" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= okCBOR >>= queryResponse >>= isReply >>= is "It works!"

    , simpleTestCase "direct signature (empty seed)" $ \cid -> do
      let userKey = genId cid ""
      req <- exampleQuery cid userKey
      sig <- genSig cid "" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= okCBOR >>= queryResponse >>= isReply >>= is "It works!"

    , simpleTestCase "direct signature (wrong seed)" $ \cid -> do
      let userKey = genId cid "Hello"
      req <- exampleQuery cid userKey
      sig <- genSig cid "Hullo" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= code4xx

    , simpleTestCase "direct signature (wrong cid)" $ \cid -> do
      let userKey = genId doesn'tExist "Hello"
      req <- exampleQuery cid userKey
      sig <- genSig cid "Hello" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= code4xx

    , simpleTestCase "direct signature (wrong root key)" $ \cid -> do
      let seed = "Hello"
      let userKey = genId cid seed
      req <- exampleQuery cid userKey
      let msg = "\x0Aic-request" <> requestId req
      -- Create the tree
      let tree = construct $
            SubTrees $ M.singleton "sig" $
            SubTrees $ M.singleton (sha256 seed) $
            SubTrees $ M.singleton (sha256 msg) $
            Value ""
      -- Create a fake certificate
      let cert_tree = construct $
            SubTrees $ M.singleton "canister" $
            SubTrees $ M.singleton cid $
            SubTrees $ M.singleton "certified_data" $
            Value (reconstruct tree)
      let fake_root_key = createSecretKeyBLS "not the root key"
      cert_sig <- sign "ic-state-root" fake_root_key (reconstruct cert_tree)
      let cert = Certificate { cert_tree, cert_sig, cert_delegation = Nothing }
      let sig = CanisterSig.genSig cert tree
      let env = simpleEnv userKey sig req []
      postQueryCBOR cid env >>= code4xx

    , simpleTestCase "delegation to Ed25519" $ \cid -> do
      let userKey = genId cid "Hello!"

      t <- getPOSIXTime
      let expiry = round ((t + 60) * 1000_000_000)
      let delegation = rec
            [ "pubkey" =: GBlob (toPublicKey otherSK)
            , "expiration" =: GNat expiry
            ]
      sig <- genSig cid "Hello!" $ "\x1Aic-request-auth-delegation" <> requestId delegation
      let signed_delegation = rec [ "delegation" =: delegation, "signature" =: GBlob sig ]

      req <- exampleQuery cid userKey
      sig <- sign "ic-request" otherSK (requestId req)
      let env = simpleEnv userKey sig req [signed_delegation]
      postQueryCBOR cid env >>= okCBOR >>= queryResponse >>= isReply >>= is "It works!"

    , simpleTestCase "delegation from Ed25519" $ \cid -> do
      let userKey = genId cid "Hello!"

      t <- getPOSIXTime
      let expiry = round ((t + 60) * 1000_000_000)
      let delegation = rec
            [ "pubkey" =: GBlob userKey
            , "expiration" =: GNat expiry
            ]
      sig <- sign "ic-request-auth-delegation" otherSK (requestId delegation)
      let signed_delegation = rec [ "delegation" =: delegation, "signature" =: GBlob sig ]

      req <- addExpiry $ rec
          [ "request_type" =: GText "query"
          , "sender" =: GBlob otherUser
          , "canister_id" =: GBlob cid
          , "method_name" =: GText "query"
          , "arg" =: GBlob (run (replyData "It works!"))
          ]
      sig <- genSig cid "Hello!" $ "\x0Aic-request" <> requestId req
      let env = simpleEnv (toPublicKey otherSK) sig req [signed_delegation]
      postQueryCBOR cid env >>= okCBOR >>= queryResponse >>= isReply >>= is "It works!"

    ]
  ]

type Blob = BS.ByteString

-- * HUnit error reporting integration

-- To use IC.HTTP.CBOR with HUnit
instance Parse IO where parseError = assertFailure . T.unpack

asRight :: HasCallStack => Either T.Text a -> IO a
asRight (Left err) = assertFailure (T.unpack err)
asRight (Right gr) = return gr

-- * Requests

-- | Posting a CBOR request, returning a raw bytestring
postCBOR :: (HasCallStack, HasTestConfig) => String -> GenR -> IO (Response BS.ByteString)
postCBOR path gr = do
    request <- parseRequest $ endPoint ++ path
    request <- return $ request
      { method = "POST"
      , requestBody = RequestBodyLBS $ BS.toLazyByteString $ encode gr
      , requestHeaders = [(hContentType, "application/cbor")]
      }
    httpLbs request testManager

-- | postCBOR with url based on effective canister id
postCallCBOR, postQueryCBOR, postReadStateCBOR :: (HasCallStack, HasTestConfig) => Blob -> GenR -> IO (Response BS.ByteString)
postCallCBOR cid      = postCBOR $ "/api/v2/canister/" ++ textual cid ++ "/call"
postQueryCBOR cid     = postCBOR $ "/api/v2/canister/" ++ textual cid ++ "/query"
postReadStateCBOR cid = postCBOR $ "/api/v2/canister/" ++ textual cid ++ "/read_state"

-- | Add envelope to CBOR request, add a nonce and expiry if it is not there,
-- post to "read", return decoded CBOR
queryCBOR :: (HasCallStack, HasTestConfig) => Blob -> GenR -> IO GenR
queryCBOR cid req = do
  addNonceExpiryEnv req >>= postQueryCBOR cid >>= okCBOR

-- | Add envelope to CBOR, and a nonce and expiry if not there, post to
-- "submit". Returns either a HTTP Error code, or if the status is 2xx, poll
-- for the request response, and return decoded CBOR
type HTTPErrOrReqResponse = Either (Int,String) ReqResponse
awaitCall' :: (HasCallStack, HasTestConfig) => Blob -> GenR -> IO HTTPErrOrReqResponse
awaitCall' cid req = do
  req <- addNonce req
  req <- addExpiry req
  res <- envelopeFor (senderOf req) req >>= postCallCBOR cid
  let code = statusCode (responseStatus res)
  if 200 <= code && code < 300
  then do
     assertBool "Response body not empty" (BS.null (responseBody res))
     Right <$> awaitStatus (senderOf req) cid (requestId req)
  else do
    let msg = T.unpack (T.decodeUtf8With T.lenientDecode (BS.toStrict (BS.take 200 (responseBody res))))
    pure $ Left (code, msg)

-- | Add envelope to CBOR, and a nonce and expiry if not there, post to
-- "submit", poll for the request response, and return decoded CBOR
awaitCall :: (HasCallStack, HasTestConfig) => Blob -> GenR -> IO ReqResponse
awaitCall cid req = awaitCall' cid req >>= is2xx

is2xx :: HasCallStack => HTTPErrOrReqResponse -> IO ReqResponse
is2xx = \case
    Left (c,msg) -> assertFailure $ "Status " ++ show c ++ " is not 2xx:\n" ++ msg
    Right res -> pure res

-- | Submits twice
awaitCallTwice :: HasTestConfig => Blob -> GenR -> IO ReqResponse
awaitCallTwice cid req = do
  req <- addNonce req
  req <- addExpiry req
  res <- envelopeFor (senderOf req) req >>= postCallCBOR cid
  code202 res
  res <- envelopeFor (senderOf req) req >>= postCallCBOR cid
  code202 res
  assertBool "Response body not empty" (BS.null (responseBody res))
  awaitStatus (senderOf req) cid (requestId req)

getStateCert' :: (HasCallStack, HasTestConfig) => Blob -> Blob -> [[Blob]] -> IO (Response Blob)
getStateCert' sender ecid paths = do
    req <- addExpiry $ rec
      [ "request_type" =: GText "read_state"
      , "sender" =: GBlob sender
      , "paths" =: GList (map (GList . map GBlob) paths)
      ]
    envelopeFor (senderOf req) req >>= postReadStateCBOR ecid

decodeCert' :: HasCallStack => Blob -> IO Certificate
decodeCert' b = either (assertFailure . T.unpack) return $ decodeCert b

getStateCert :: (HasCallStack, HasTestConfig) => Blob -> Blob -> [[Blob]] -> IO Certificate
getStateCert sender ecid paths = do
    gr <- getStateCert' sender ecid paths >>= okCBOR
    b <- record (field blob "certificate") gr
    cert <- decodeCert' b

    case wellFormed (cert_tree cert) of
        Left err -> assertFailure $ "Hash tree not well formed: " ++ err
        Right () -> return ()

    return cert

extractCertData :: Blob -> Blob -> IO Blob
extractCertData cid b = do
  cert <- decodeCert' b
  case wellFormed (cert_tree cert) of
      Left err -> assertFailure $ "Hash tree not well formed: " ++ err
      Right () -> return ()
  certValue cert ["canister", cid, "certified_data"]

verboseVerify :: String -> Blob -> Blob -> Blob -> Blob -> IO ()
verboseVerify what domain_sep pk msg sig =
    case DER_BLS.verify domain_sep pk msg sig of
        Left err -> assertFailure $ unlines
            [ "Signature verification failed on " ++ what
            , T.unpack err
            , "Domain separator:   " ++ prettyBlob domain_sep
            , "Public key (DER):   " ++ asHex pk
            , "Public key decoded: " ++
               case DER.decode pk of
                 Left err -> T.unpack err
                 Right (suite, key) -> asHex key ++ " (" ++ show suite ++ ")"
            , "Signature:          " ++ asHex sig
            , "Checked message:    " ++ prettyBlob msg
            ]
        Right () -> return ()

validateDelegation :: (HasCallStack, HasTestConfig) => Maybe Delegation -> IO Blob
validateDelegation Nothing = return (tc_root_key testConfig)
validateDelegation (Just del) = do
    cert <- decodeCert' (del_certificate del)
    case wellFormed (cert_tree cert) of
        Left err -> assertFailure $ "Hash tree not well formed: " ++ err
        Right () -> return ()
    validateStateCert' "certificate delegation" cert

    certValue cert ["subnet", del_subnet_id del, "public_key"]

validateStateCert' :: (HasCallStack, HasTestConfig) => String -> Certificate -> IO ()
validateStateCert' what cert = do
    pk <- validateDelegation (cert_delegation cert)
    verboseVerify what "ic-state-root" pk (reconstruct (cert_tree cert)) (cert_sig cert)

validateStateCert :: (HasCallStack, HasTestConfig) => Certificate -> IO ()
validateStateCert = validateStateCert' "certificate"

data ReqResponse = Reply Blob | Reject Natural T.Text
  deriving (Eq, Show)
data ReqStatus = Processing | Pending | Responded ReqResponse | UnknownStatus
  deriving (Eq, Show)

prettyPath :: [Blob] -> String
prettyPath = concatMap (("/" ++) . shorten 15 . prettyBlob)

prettyBlob :: Blob -> String
prettyBlob x =
  let s = map (chr . fromIntegral) (BS.unpack x) in
  if all isPrint s then s else asHex x

certValue :: HasCallStack => CertVal a => Certificate -> [Blob] -> IO a
certValue cert path = case lookupPath (cert_tree cert) path of
    Found b -> case fromCertVal b of
      Just x -> return x
      Nothing -> assertFailure $ "Cannot parse " ++ prettyPath path ++ " from " ++ show b
    x -> assertFailure $ "Expected to find " ++ prettyPath path ++ ", but got " ++ show x

certValueAbsent :: HasCallStack => Certificate -> [Blob] -> IO ()
certValueAbsent cert path = case lookupPath (cert_tree cert) path of
    Absent -> return ()
    x -> assertFailure $ "Path " ++ prettyPath path ++ " should be absent, but got " ++ show x

getRequestStatus :: (HasCallStack, HasTestConfig) => Blob -> Blob -> Blob -> IO ReqStatus
getRequestStatus sender cid rid = do
    cert <- getStateCert sender cid [["request_status", rid]]

    case lookupPath (cert_tree cert) ["request_status", rid, "status"] of
      Absent -> return UnknownStatus
      Found "processing" -> return Processing
      Found "received" -> return Pending
      Found "replied" -> do
        b <- certValue cert ["request_status", rid, "reply"]
        certValueAbsent cert ["request_status", rid, "reject_code"]
        certValueAbsent cert ["request_status", rid, "reject_message"]
        return $ Responded (Reply b)
      Found "rejected" -> do
        certValueAbsent cert ["request_status", rid, "reply"]
        code <- certValue cert ["request_status", rid, "reject_code"]
        msg <- certValue cert ["request_status", rid, "reject_message"]
        return $ Responded (Reject code msg)
      Found s -> assertFailure $ "Unexpected status " ++ show s
      -- This case should not happen with a compliant IC, but let
      -- us be liberal here, and strict in a dedicated test
      Unknown -> return UnknownStatus
      x -> assertFailure $ "Unexpected request status, got " ++ show x

awaitStatus :: HasTestConfig => Blob -> Blob -> Blob -> IO ReqResponse
awaitStatus sender cid rid = loop $ pollDelay >> getRequestStatus sender cid rid >>= \case
    Responded x -> return $ Just x
    _ -> return Nothing
  where
    loop :: HasCallStack => IO (Maybe a) -> IO a
    loop act = go (0::Int)
      where
        go 10000 = assertFailure "Polling timed out"
        go n = act >>= \case
          Just r -> return r
          Nothing -> go (n+1)

pollDelay :: IO ()
pollDelay = threadDelay $ 10 * 1000 -- 10 milliseonds

-- How long to wait before checking if a request that should _not_ show up on
-- the system indeed did not show up
ingressDelay :: IO ()
ingressDelay = threadDelay $ 2 * 1000 * 1000 -- 2 seconds


-- * HTTP Response predicates

codePred :: HasCallStack => String -> (Int -> Bool) -> Response Blob -> IO ()
codePred expt pred response = assertBool
    ("Status " ++ show c ++ " is not " ++ expt ++ "\n" ++ msg)
    (pred c)
  where
    c = statusCode (responseStatus response)
    msg = T.unpack (T.decodeUtf8With T.lenientDecode (BS.toStrict (BS.take 200 (responseBody response))))

code2xx, code202, code4xx, code202_or_4xx  :: HasCallStack => Response BS.ByteString -> IO ()
code2xx = codePred "2xx" $ \c -> 200 <= c && c < 300
code202 = codePred "202" $ \c -> c == 202
code4xx = codePred "4xx" $ \c -> 400 <= c && c < 500
code202_or_4xx = codePred "202 or 4xx" $ \c -> c == 202 || 400 <= c && c < 500

-- * CBOR decoding

okCBOR :: HasCallStack => Response BS.ByteString -> IO GenR
okCBOR response = do
  code2xx response
  asRight $ decode $ responseBody response

-- * Response predicates and parsers

queryResponse :: GenR -> IO ReqResponse
queryResponse = record $ do
    s <- field text "status"
    case s of
      "replied" ->
        Reply <$> field (record (field blob "arg")) "reply"
      "rejected" -> do
        code <- field nat "reject_code"
        msg <- field text "reject_message"
        return $ Reject code msg
      _ -> lift $ assertFailure $ "Unexpected status " ++ show s

isReject :: HasCallStack => [Natural] -> ReqResponse -> IO ()
isReject _ (Reply r) =
  assertFailure $ "Expected reject, got reply:" ++ prettyBlob r
isReject codes (Reject n msg) = do
  assertBool
    ("Reject code " ++ show n ++ " not in " ++ show codes ++ "\n" ++ T.unpack msg)
    (n `elem` codes)

isErrOrReject :: HasCallStack => [Natural] -> HTTPErrOrReqResponse -> IO ()
isErrOrReject _codes (Left (c, msg))
    | 400 <= c && c < 600 = return ()
    | otherwise = assertFailure $
        "Status " ++ show c ++ " is not 4xx or 5xx:\n" ++ msg
isErrOrReject [] (Right _) = assertFailure "Got HTTP response, expected HTTP error"
isErrOrReject codes (Right res) = isReject codes res


isReply :: HasCallStack => ReqResponse -> IO Blob
isReply (Reply b) = return b
isReply (Reject n msg) =
  assertFailure $ "Unexpected reject (code " ++ show n ++ "): " ++ T.unpack msg

-- Predicates to handle the responses from relayReply and relayReject
isRelay :: HasCallStack => Blob -> IO ReqResponse
isRelay = runGet $ Get.getWord32le >>= \case
    0 -> Reply <$> Get.getRemainingLazyByteString
    0x4c444944 -> fail "Encountered Candid when expectin relayed data. Did you forget to use isRelay?"
    c -> do
      msg <- Get.getRemainingLazyByteString
      return $ Reject (fromIntegral c) (T.decodeUtf8With T.lenientDecode (BS.toStrict msg))

-- Convenience decoders
asWord32 :: HasCallStack => Blob -> IO Word32
asWord32 = runGet Get.getWord32le

asWord64 :: HasCallStack => Blob -> IO Word64
asWord64 = runGet Get.getWord64le

as2Word64 :: HasCallStack => Blob -> IO (Word64, Word64)
as2Word64 = runGet $ (,) <$> Get.getWord64le <*> Get.getWord64le

bothSame :: (Eq a, Show a) => (a, a) -> Assertion
bothSame (x,y) = x @?= y

runGet :: HasCallStack => Get.Get a -> Blob -> IO a
runGet a b = case  Get.runGetOrFail (a <* done) b of
    Left (_,_, err) ->
        fail $ "Could not parse " ++ show b ++ ": " ++ err
    Right (_,_, x) -> return x
  where
    done = do
        nothing_left <- Get.isEmpty
        unless nothing_left (fail "left-over bytes")

is :: (HasCallStack, Eq a, Show a) => a -> a -> Assertion
is exp act = act @?= exp

data StatusResponse = StatusResponse
    { status_api_version :: T.Text
    , status_root_key :: Blob
    }

statusResonse :: HasCallStack => GenR -> IO StatusResponse
statusResonse = record $ do
    v <- field text "ic_api_version"
    _ <- optionalField text "impl_source"
    _ <- optionalField text "impl_version"
    _ <- optionalField text "impl_revision"
    pk <- field blob "root_key"
    swallowAllFields -- More fields are explicitly allowed
    return StatusResponse {status_api_version = v, status_root_key = pk }

-- * Interacting with aaaaa-aa (via HTTP)

{-
The code below has some repetition. That’s because we have

 A) multiple ways of _calling_ the Management Canister
    (as default user, as specific user, via canister, with or without cycles),
 B) different things we want to know
    (just the Candid-decoded reply, or the response, or even the HTTP error)
 C) and then of course different methods (which affect response decoding)

So far, there is some duplication here because of that. Eventually, this should
be refactored so that the test can declarative pick A, B and C separately.
-}

-- how to reach the management canister
type IC00 = Blob -> T.Text -> Blob -> IO ReqResponse

ic00as :: (HasTestConfig, HasCallStack) => Blob -> IC00
ic00as user ecid method_name arg = awaitCall ecid $ rec
      [ "request_type" =: GText "call"
      , "sender" =: GBlob user
      , "canister_id" =: GBlob ""
      , "method_name" =: GText method_name
      , "arg" =: GBlob arg
      ]

ic00 :: HasTestConfig => IC00
ic00 = ic00as defaultUser

ic00via :: HasTestConfig => Blob -> IC00
ic00via cid = ic00viaWithCycles cid 0

ic00viaWithCycles :: HasTestConfig => Blob -> Word64 -> IC00
ic00viaWithCycles cid cycles _ecid method_name arg =
  do call' cid $
      callNew
        (bytes "") (bytes (BS.fromStrict (T.encodeUtf8 method_name))) -- aaaaa-aa
        (callback relayReply) (callback relayReject) >>>
      callDataAppend (bytes arg) >>>
      callCyclesAdd (int64 cycles) >>>
      callPerform
   >>= isReply >>= isRelay

-- A variant that allows non-200 responses to submit
ic00as' :: HasTestConfig => Blob -> Blob -> T.Text -> Blob -> IO HTTPErrOrReqResponse
ic00as' user cid method_name arg = awaitCall' cid $ rec
      [ "request_type" =: GText "call"
      , "sender" =: GBlob user
      , "canister_id" =: GBlob ""
      , "method_name" =: GText method_name
      , "arg" =: GBlob arg
      ]

-- Now wrapping the concrete calls
-- (using Candid.toCandidService is tricky because of all stuff like passing through the effective canister id)
--
callIC :: forall s a b.
  (HasCallStack, HasTestConfig) =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  (Candid.CandidArg a, Candid.CandidArg b) =>
  IC00 -> Blob -> Label s -> a -> IO b
callIC ic00 ecid l x = do
    r <- ic00 ecid (T.pack (symbolVal l)) (Candid.encode x) >>= isReply
    case Candid.decode r of
        Left err -> assertFailure $ "Candid decoding error: " ++ err
        Right y -> pure y

-- The following line noise is me getting out of my way
-- to be able to use `ic_create` etc. by passing a record that contains
-- a subset of settings, without Maybe
type family UnRec r where UnRec (R.Rec r) = r
type PartialSettings r = (R.Forall r R.Unconstrained1, R.Map Maybe r .// UnRec Settings ≈ UnRec Settings)
fromPartialSettings :: PartialSettings r => R.Rec r -> Settings
fromPartialSettings r =
    R.map' Just r .//
    R.default' @(R.IsA R.Unconstrained1 Maybe) @(UnRec Settings) d
  where
    d :: forall a. R.IsA R.Unconstrained1 Maybe a => a
    d = case R.as @R.Unconstrained1 @Maybe @a of R.As -> Nothing

ic_create :: (HasCallStack, HasTestConfig, PartialSettings r) => IC00 -> Rec r -> IO Blob
ic_create ic00 ps = do
  r <- callIC ic00 "" #create_canister $ empty
    .+ #settings .== Just (fromPartialSettings ps)
  return (rawPrincipal (r .! #canister_id))

ic_provisional_create ::
    (HasCallStack, HasTestConfig, PartialSettings r) =>
    IC00 -> Maybe Natural -> Rec r -> IO Blob
ic_provisional_create ic00 cycles ps = do
  r <- callIC ic00 "" #provisional_create_canister_with_cycles $ empty
    .+ #amount .== cycles
    .+ #settings .== Just (fromPartialSettings ps)
  return (rawPrincipal (r .! #canister_id))

ic_install :: (HasCallStack, HasTestConfig) => IC00 -> InstallMode -> Blob -> Blob -> Blob -> IO ()
ic_install ic00 mode canister_id wasm_module arg = do
  callIC ic00 canister_id #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg

ic_uninstall :: (HasCallStack, HasTestConfig) => IC00 -> Blob -> IO ()
ic_uninstall ic00 canister_id = do
  callIC ic00 canister_id #uninstall_code $ empty
    .+ #canister_id .== Principal canister_id

ic_set_controller :: HasTestConfig => IC00 -> Blob -> Blob -> IO ()
ic_set_controller ic00 canister_id new_controller = do
  callIC ic00 canister_id #update_settings $ empty
    .+ #canister_id .== Principal canister_id
    .+ #settings .== fromPartialSettings (#controller .== Principal new_controller)

ic_start_canister :: HasTestConfig => IC00 -> Blob -> IO ()
ic_start_canister ic00 canister_id = do
  callIC ic00 canister_id #start_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_stop_canister :: HasTestConfig => IC00 -> Blob -> IO ()
ic_stop_canister ic00 canister_id = do
  callIC ic00 canister_id #stop_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_canister_status ::
    forall a b. (a -> IO b) ~ (ICManagement IO .! "canister_status") =>
    HasTestConfig => IC00 -> Blob -> IO b
ic_canister_status ic00 canister_id = do
  callIC ic00 canister_id #canister_status $ empty
    .+ #canister_id .== Principal canister_id

ic_deposit_cycles :: HasTestConfig => IC00 -> Blob -> IO ()
ic_deposit_cycles ic00 canister_id = do
  callIC ic00 canister_id #deposit_cycles $ empty
    .+ #canister_id .== Principal canister_id

ic_top_up :: HasTestConfig => IC00 -> Blob -> Natural -> IO ()
ic_top_up ic00 canister_id amount = do
  callIC ic00 canister_id #provisional_top_up_canister $ empty
    .+ #canister_id .== Principal canister_id
    .+ #amount .== amount

ic_delete_canister :: HasTestConfig => IC00 -> Blob -> IO ()
ic_delete_canister ic00 canister_id = do
  callIC ic00 canister_id #delete_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_raw_rand :: HasTestConfig => IC00 -> IO Blob
ic_raw_rand ic00 =
  callIC ic00 "" #raw_rand ()

-- Primed variants return the response (reply or reject)
callIC' :: forall s a b.
  HasTestConfig =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  IC00 -> Blob -> Label s -> a -> IO ReqResponse
callIC' ic00 ecid l x = ic00 ecid (T.pack (symbolVal l)) (Candid.encode x)

ic_create' ::
    (HasCallStack, HasTestConfig, PartialSettings r) =>
    IC00 -> Rec r -> IO ReqResponse
ic_create' ic00 ps = do
  callIC' ic00 "" #create_canister $ empty
    .+ #settings .== Just (fromPartialSettings ps)

ic_provisional_create' ::
    (HasCallStack, HasTestConfig, PartialSettings r) =>
    IC00 -> Maybe Natural -> Rec r -> IO ReqResponse
ic_provisional_create' ic00 cycles ps = do
  callIC' ic00 "" #provisional_create_canister_with_cycles $ empty
    .+ #amount .== cycles
    .+ #settings .== Just (fromPartialSettings ps)

ic_install' :: HasTestConfig => IC00 -> InstallMode -> Blob -> Blob -> Blob -> IO ReqResponse
ic_install' ic00 mode canister_id wasm_module arg =
  callIC' ic00 canister_id #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg

ic_update_settings' :: (HasTestConfig, PartialSettings r) => IC00 -> Blob -> Rec r -> IO ReqResponse
ic_update_settings' ic00 canister_id r = do
  callIC' ic00 canister_id #update_settings $ empty
    .+ #canister_id .== Principal canister_id
    .+ #settings .== fromPartialSettings r

ic_set_controller' :: HasTestConfig => IC00 -> Blob -> Blob -> IO ReqResponse
ic_set_controller' ic00 canister_id new_controller = do
  ic_update_settings' ic00 canister_id (#controller .== Principal new_controller)

ic_delete_canister' :: HasTestConfig => IC00 -> Blob -> IO ReqResponse
ic_delete_canister' ic00 canister_id = do
  callIC' ic00 canister_id #delete_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_deposit_cycles' :: HasTestConfig => IC00 -> Blob -> IO ReqResponse
ic_deposit_cycles' ic00 canister_id = do
  callIC' ic00 canister_id #deposit_cycles $ empty
    .+ #canister_id .== Principal canister_id

ic_top_up' :: HasTestConfig => IC00 -> Blob -> Natural -> IO ReqResponse
ic_top_up' ic00 canister_id amount = do
  callIC' ic00 canister_id #provisional_top_up_canister $ empty
    .+ #canister_id .== Principal canister_id
    .+ #amount .== amount

-- Double primed variants are only for requests from users (so they take the user,
-- not a generic ic00 thing), and return the HTTP error code or the response
-- (reply or reject)

callIC'' :: forall s a b.
  HasTestConfig =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  Blob -> Blob -> Label s -> a -> IO HTTPErrOrReqResponse
callIC'' user ecid l x = ic00as' user ecid (T.pack (symbolVal l)) (Candid.encode x)

ic_install'' :: (HasCallStack, HasTestConfig) => Blob -> InstallMode -> Blob -> Blob -> Blob -> IO HTTPErrOrReqResponse
ic_install'' user mode canister_id wasm_module arg =
  callIC'' user canister_id #install_code $ empty
    .+ #mode .== mode
    .+ #canister_id .== Principal canister_id
    .+ #wasm_module .== wasm_module
    .+ #arg .== arg

ic_uninstall'' :: HasTestConfig => Blob -> Blob -> IO HTTPErrOrReqResponse
ic_uninstall'' user canister_id =
  callIC'' user canister_id #uninstall_code $ empty
    .+ #canister_id .== Principal canister_id


ic_set_controller'' :: HasTestConfig => Blob -> Blob -> Blob -> IO HTTPErrOrReqResponse
ic_set_controller'' user canister_id new_controller = do
  callIC'' user canister_id #update_settings $ empty
    .+ #canister_id .== Principal canister_id
    .+ #settings .== fromPartialSettings (#controller .== Principal new_controller)

ic_start_canister'' :: HasTestConfig => Blob -> Blob -> IO HTTPErrOrReqResponse
ic_start_canister'' user canister_id = do
  callIC'' user canister_id #start_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_stop_canister'' :: HasTestConfig => Blob -> Blob -> IO HTTPErrOrReqResponse
ic_stop_canister'' user canister_id = do
  callIC'' user canister_id #stop_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_canister_status'' :: HasTestConfig => Blob -> Blob -> IO HTTPErrOrReqResponse
ic_canister_status'' user canister_id = do
  callIC'' user canister_id #canister_status $ empty
    .+ #canister_id .== Principal canister_id

ic_delete_canister'' :: HasTestConfig => Blob -> Blob -> IO HTTPErrOrReqResponse
ic_delete_canister'' user canister_id = do
  callIC'' user canister_id #delete_canister $ empty
    .+ #canister_id .== Principal canister_id

ic_deposit_cycles'' :: HasTestConfig => Blob -> Blob -> IO HTTPErrOrReqResponse
ic_deposit_cycles'' user canister_id = do
  callIC'' user canister_id #deposit_cycles $ empty
    .+ #canister_id .== Principal canister_id

ic_raw_rand'' :: HasTestConfig => Blob -> IO HTTPErrOrReqResponse
ic_raw_rand'' user = do
  callIC'' user "" #raw_rand ()


-- A barrier

-- This will stop and start all mentioned canisters. This guarantees
-- that all outstanding callbacks are handled
barrier :: HasTestConfig => [Blob] -> IO ()
barrier cids = do
  mapM_ (ic_stop_canister ic00) cids
  mapM_ (ic_start_canister ic00) cids

-- * Interacting with the universal canister

-- Some common operations on the universal canister
-- The primed variant (call') return the response record,
-- e.g. to check for error conditions.
-- The unprimed variant expect a reply.

install' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
install' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #install) cid universal_wasm (run prog)

installAt :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
installAt cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install ic00 (enum #install) cid universal_wasm (run prog)

-- Also calls create, used default 'ic00'
install :: (HasCallStack, HasTestConfig) => Prog -> IO Blob
install prog = do
    cid <- create
    installAt cid prog
    return cid

create :: (HasCallStack, HasTestConfig) => IO Blob
create = ic_provisional_create ic00 Nothing empty

upgrade' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
upgrade' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #upgrade) cid universal_wasm (run prog)

upgrade :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
upgrade cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install ic00 (enum #upgrade) cid universal_wasm (run prog)

reinstall' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
reinstall' cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install' ic00 (enum #reinstall) cid universal_wasm (run prog)

reinstall :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
reinstall cid prog = do
  universal_wasm <- getTestWasm "universal_canister"
  ic_install ic00 (enum #reinstall) cid universal_wasm (run prog)

callRequestAs :: (HasCallStack, HasTestConfig) => Blob -> Blob -> Prog -> GenR
callRequestAs user cid prog = rec
    [ "request_type" =: GText "call"
    , "sender" =: GBlob user
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "update"
    , "arg" =: GBlob (run prog)
    ]

callToQueryRequestAs :: (HasCallStack, HasTestConfig) => Blob -> Blob -> Prog -> GenR
callToQueryRequestAs user cid prog = rec
    [ "request_type" =: GText "call"
    , "sender" =: GBlob user
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "query"
    , "arg" =: GBlob (run prog)
    ]

callRequest :: (HasCallStack, HasTestConfig) => Blob -> Prog -> GenR
callRequest cid prog = callRequestAs defaultUser cid prog

callToQuery'' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO HTTPErrOrReqResponse
callToQuery'' cid prog = awaitCall' cid $ callToQueryRequestAs defaultUser cid prog

-- The following variants of the call combinator differ in how much failure they allow:
--
--   call'' allows HTTP errors at `submit` time already
--   call'  requires submission to succeed, and allows reject responses
--   call   requires a reply response
--   call_  requires a reply response with an empty blob (a common case)

call'' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO HTTPErrOrReqResponse
call'' cid prog = awaitCall' cid (callRequest cid prog)

call' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
call' cid prog = call'' cid prog >>= is2xx

call :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO Blob
call cid prog = call' cid prog >>= isReply

call_ :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
call_ cid prog = call cid prog >>= is ""

callTwice' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
callTwice' cid prog = awaitCallTwice cid (callRequest cid prog)


query' :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ReqResponse
query' cid prog =
  queryCBOR cid >=> queryResponse $ rec
    [ "request_type" =: GText "query"
    , "sender" =: GBlob defaultUser
    , "canister_id" =: GBlob cid
    , "method_name" =: GText "query"
    , "arg" =: GBlob (run prog)
    ]

query :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO Blob
query cid prog = query' cid prog >>= isReply

query_ :: (HasCallStack, HasTestConfig) => Blob -> Prog -> IO ()
query_ cid prog = query cid prog >>= is ""

-- Shortcut for test cases that just need one canister
simpleTestCase :: (HasCallStack, HasTestConfig) => String -> (Blob -> IO ()) -> TestTree
simpleTestCase name act = testCase name $ install noop >>= act

-- * Programmatic test generation

-- | Runs test once for each field with that field removed, including nested
-- fields
omitFields :: GenR -> (GenR -> Assertion) -> [TestTree]
omitFields (GRec hm) act =
    [ let hm' = HM.delete f hm
      in testCase ("omitting " ++ T.unpack f) $ act (GRec hm')
    | f <- fields
    ] ++ concat
    [ omitFields val $ \val' -> act (GRec (HM.insert f val' hm))
    | f <- fields
    , val@(GRec _) <- return $ hm HM.! f
    ]
  where fields = sort $ HM.keys hm
omitFields _ _ = error "omitFields needs a GRec"


getRand8Bytes :: IO BS.ByteString
getRand8Bytes = BS.pack <$> replicateM 8 randomIO

-- * Endpoint handling

-- Yes, implicit arguments are frowned upon. But they are also very useful.

type HasTestConfig = (?testConfig :: TestConfig)

withTestConfig :: (forall. HasTestConfig => a) -> TestConfig -> a
withTestConfig act tc = let ?testConfig = tc in act

testConfig :: HasTestConfig => TestConfig
testConfig = ?testConfig

endPoint :: HasTestConfig => String
endPoint = tc_endPoint testConfig

testManager :: HasTestConfig => Manager
testManager = tc_manager testConfig

-- * Test data access

getTestFile :: FilePath -> IO FilePath
getTestFile file =
    lookupEnv "IC_TEST_DATA" >>= \case
    Just fp -> return $ fp </> file
    Nothing -> do
      -- nix use
      exePath <- getExecutablePath
      let exeRelPath = takeDirectory exePath </> "../test-data"
      -- convenient for cabal new-run use
      try [ exeRelPath, "test-data", "../test-data", "impl/test-data" ]
  where
    try (d:ds) = doesFileExist (d </> file) >>= \case
      True -> return (d </> file)
      False -> try ds
    try [] = error $ "getTestDir: Could not read " ++ file ++ " from test-data/. Please consult impl/README.md"

getTestWasm :: FilePath -> IO BS.ByteString
getTestWasm base = do
  fp <- getTestFile $ base <.> "wasm"
  BS.readFile fp


-- Convenience around Data.Row.Variants used as enums

enum :: (AllUniqueLabels r, KnownSymbol l, (r .! l) ~ ()) => Label l -> Var r
enum l = V.IsJust l ()

-- Other utilities

asHex :: Blob -> String
asHex = T.unpack . H.encodeHex . BS.toStrict

textual :: Blob -> String
textual = T.unpack . prettyPrincipal . Principal


shorten :: Int -> String -> String
shorten n s = a ++ (if null b then "" else "…")
  where (a,b) = splitAt n s
