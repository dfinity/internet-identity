{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Options.Applicative hiding (empty)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Hex as H
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Vector as V
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.Proxy
import Data.Word
import Control.Monad.Random.Lazy
import Data.Time.Clock.POSIX
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Runners.AntXML
import Test.Tasty.Options
import Test.Tasty.Runners.Html

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Row (empty, (.==), (.+), type (.!), (.!), Label)
import qualified Codec.Candid as Candid
import qualified Data.Row.Variants as V

import IC.Types
import IC.Ref
import IC.Management
import IC.Crypto
import IC.Id.Forms hiding (Blob)
import IC.HTTP.GenR
import IC.HTTP.RequestId



-- copied from ./src/idp_service/idp_service.did,
-- and then modified to replace vec nat8 with blob
type IDPInterface m = [Candid.candid|


type UserNumber = nat64;
type PublicKey = blob;
type CredentialId = blob;
type DeviceAlias = text;
type DeviceKey = PublicKey;
type UserKey = PublicKey;
type SessionKey = PublicKey;
type FrontendHostname = text;
type Timestamp = nat64;

type DeviceData = record {
  pubkey : DeviceKey;
  alias : text;
  credential_id : opt CredentialId;
};

type Delegation = record {
  pubkey: SessionKey;
  expiration: Timestamp;
  targets: opt vec principal;
};
type SignedDelegation = record {
  delegation: Delegation;
  signature: blob;
};
type GetDelegationResponse = variant {
  signed_delegation: SignedDelegation;
  no_such_delegation;
};

type InternetIdentityStats = record {
  users_registered: nat64;
  assigned_user_number_range: record { nat64; nat64; };
};

type ProofOfWork = record {
  timestamp : Timestamp;
  nonce : nat64;
};

service : {
  register : (DeviceData, ProofOfWork) -> (UserNumber);
  add : (UserNumber, DeviceData) -> ();
  remove : (UserNumber, DeviceKey) -> ();
  lookup : (UserNumber) -> (vec DeviceData) query;
  stats : () -> (InternetIdentityStats) query;

  prepare_delegation : (UserNumber, FrontendHostname, SessionKey) -> (UserKey, Timestamp);
  get_delegation: (UserNumber, FrontendHostname, SessionKey, Timestamp) -> (GetDelegationResponse) query;
}
  |]

-- Names for some of these types. Unfortunately requires copying

type DeviceData = [Candid.candidType|
record {
  pubkey : blob;
  alias : text;
  credential_id : opt blob;
}
  |]


type SignedDelegation = [Candid.candidType|
record {
  delegation:
    record {
      pubkey: blob;
      expiration: nat64;
      targets: opt vec principal;
    };
  signature: blob;
}
  |]

type Delegation = [Candid.candidType|
  record {
    pubkey: blob;
    expiration: nat64;
    targets: opt vec principal;
  }
  |]


type InitCandid = [Candid.candidType|
  record {
    assigned_user_number_range : record { nat64; nat64; };
  }
  |]
type Init = (Word64, Word64)
toInitCandid :: Init -> Maybe InitCandid
toInitCandid (l,u) = Just $ #assigned_user_number_range .== (#_0_ .== l .+ #_1_ .== u)

type ProofOfWork = [Candid.candidType|record {
  timestamp : nat64;
  nonce : nat64;
}|]

mkPOW :: Word64 -> Word64 -> ProofOfWork
mkPOW t n = #timestamp .== t .+ #nonce .== n


type Hash = Blob

delegationHash :: Delegation -> Hash
delegationHash d = requestId $ rec $
  [ "pubkey" =: GBlob (d .! #pubkey)
  , "expiration" =: GNat (fromIntegral (d .! #expiration))
  ] ++
  [ "targets" =: GList [ GBlob b | Candid.Principal b <- V.toList ts ]
  | Just ts <- pure $ d .! #targets
  ]


type M = StateT IC IO

dummyUserId :: CanisterId
dummyUserId = EntityId $ BS.pack [0xCA, 0xFF, 0xEE]

controllerId :: CanisterId
controllerId = EntityId "I am the controller"

shorten :: Int -> String -> String
shorten n s = a ++ (if null b then "" else "…")
  where (a,b) = splitAt n s


submitAndRun :: CallRequest -> M CallResponse
submitAndRun r = do
    rid <- lift mkRequestId
    submitRequest rid r
    runToCompletion
    r <- gets (snd . (M.! rid) . requests)
    case r of
      CallResponse r -> return r
      _ -> lift $ assertFailure $ "submitRequest: request status is still " <> show r

submitQuery :: QueryRequest -> M CallResponse
submitQuery r = do
    t <- lift getTimestamp
    QueryResponse r <- handleQuery t r
    return r
  where
    getTimestamp :: IO Timestamp
    getTimestamp = do
        t <- getPOSIXTime
        return $ Timestamp $ round (t * 1000_000_000)

mkRequestId :: IO RequestID
mkRequestId = BS.toLazyByteString . BS.word64BE <$> randomIO

setCanisterTimeTo :: Blob -> Timestamp -> M ()
setCanisterTimeTo cid new_time =
 modify $
  \ic -> ic { canisters = M.adjust (\cs -> cs { time = new_time }) (EntityId cid) (canisters ic) }


callManagement :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (ICManagement IO .! s) =>
  Candid.CandidArg a =>
  Candid.CandidArg b =>
  EntityId -> Label s -> a -> M b
callManagement user_id l x = do
  r <- submitAndRun $
    CallRequest (EntityId mempty) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected (_code, msg) -> lift $ assertFailure $ "Management call got rejected:\n" <> msg
    Replied b -> case Candid.decode b of
      Left err -> lift $ assertFailure err
      Right y -> return y

queryIDP :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IDPInterface IO .! s) =>
  Candid.CandidArg a =>
  Candid.CandidArg b =>
  BS.ByteString -> EntityId -> Label s -> a -> M b
queryIDP cid user_id l x = do
  r <- submitQuery $ QueryRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected (_code, msg) -> lift $ assertFailure $ "IDP query got rejected:\n" <> msg
    Replied b -> case Candid.decode b of
      Left err -> lift $ assertFailure err
      Right y -> return y

queryIDPReject :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IDPInterface IO .! s) =>
  Candid.CandidArg a =>
  BS.ByteString -> EntityId -> Label s -> a -> M ()
queryIDPReject cid user_id l x = do
  r <- submitQuery $ QueryRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected _ -> return ()
    Replied _ -> lift $ assertFailure "queryIDPReject: Unexpected reply"

callIDP :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IDPInterface IO .! s) =>
  Candid.CandidArg a =>
  Candid.CandidArg b =>
  BS.ByteString -> EntityId -> Label s -> a -> M b
callIDP cid user_id l x = do
  r <- submitAndRun $ CallRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected (_code, msg) -> lift $ assertFailure $ "IDP call got rejected:\n" <> msg
    Replied b -> case Candid.decode b of
      Left err -> lift $ assertFailure err
      Right y -> return y

callIDPReject :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IDPInterface IO .! s) =>
  Candid.CandidArg a =>
  BS.ByteString -> EntityId -> Label s -> a -> M ()
callIDPReject cid user_id l x = do
  r <- submitAndRun $
    CallRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected _ -> return ()
    Replied _ -> lift $ assertFailure "callIDPReject: Unexpected reply"


-- Some common devices
webauthSK :: SecretKey
webauthSK = createSecretKeyWebAuthn "foobar"
webauthPK :: PublicKey
webauthPK = toPublicKey webauthSK
webauthID :: EntityId
webauthID = EntityId $ mkSelfAuthenticatingId webauthPK
device1 :: DeviceData
device1 = empty
    .+ #alias .== "device1"
    .+ #pubkey .== webauthPK
    .+ #credential_id .== Nothing

webauth2SK :: SecretKey
webauth2SK = createSecretKeyWebAuthn "foobar2"
webauth2PK = toPublicKey webauth2SK
webauth2PK :: PublicKey
webauth2ID :: EntityId
webauth2ID = EntityId $ mkSelfAuthenticatingId webauth2PK
device2 :: DeviceData
device2 = empty
    .+ #alias .== "device2"
    .+ #pubkey .== webauth2PK
    .+ #credential_id .== Just "foobar"

-- Various proof of work values
invalidPOW :: ProofOfWork
invalidPOW = mkPOW 0 0

-- Hardcoded solutions for the POW puzzle
powNonceAt :: Blob -> Word64 -> Word64
powNonceAt "\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\SOH" 0 = 57583
powNonceAt "\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\SOH" 1 = 40219
powNonceAt "\NUL\EOT\NUL\NUL\NUL\NUL\NUL\NUL\SOH" 1200000000000 = 104906
powNonceAt cid ts = error $ printf
    "No proof of work on record. Please run\nnpx ts-node pow.ts %s %d\nand add to this table as\npowNonceAt %s %d = <paste number here>"
    (asHex cid) ts
    (show cid) ts

powAt ::  Blob -> Word64 -> ProofOfWork
powAt cid ts = mkPOW ts (powNonceAt cid ts)

lookupIs :: Blob -> Word64 -> [DeviceData] -> M ()
lookupIs cid user_number ds = do
  r <- queryIDP cid dummyUserId #lookup user_number
  liftIO $ r @?= V.fromList ds

addTS :: (a, b, c) -> d -> (a, b, c, d)
addTS (a,b,c) ts = (a,b,c,ts)

getAndValidate :: HasCallStack => Blob -> Blob -> Blob -> EntityId -> (Word64, T.Text, Blob) -> Word64 -> M ()
getAndValidate cid sessionPK userPK webauthID delegationArgs ts = do
  sd <- queryIDP cid webauthID #get_delegation (addTS delegationArgs ts) >>= \case
    V.IsJust (V.Label :: Label "signed_delegation") sd -> return sd
    V.IsJust (V.Label :: Label "no_such_delegation") () ->
        liftIO $ assertFailure "Got unexpected no_such_delegation"
    _ -> error "unreachable"
  let delegation = sd .! #delegation
  let sig = sd .! #signature
  lift $ delegation .! #pubkey @?= sessionPK

  root_key <- gets $ toPublicKey . secretRootKey
  case verify root_key "ic-request-auth-delegation" userPK (delegationHash delegation) sig of
    Left err -> liftIO $ assertFailure $ T.unpack err
    Right () -> return ()

getButNotThere :: HasCallStack => Blob -> EntityId -> (Word64, T.Text, Blob) -> Word64 -> M ()
getButNotThere cid webauthID delegationArgs ts = do
  queryIDP cid webauthID #get_delegation (addTS delegationArgs ts) >>= \case
    V.IsJust (V.Label :: Label "signed_delegation") _ ->
        liftIO $ assertFailure "Unexpected delegation"
    V.IsJust (V.Label :: Label "no_such_delegation") () -> return ()
    _ -> error "unreachable"

assertStats :: HasCallStack => Blob -> Word64 -> M()
assertStats cid expUsers = do
  s <- queryIDP cid dummyUserId #stats ()
  lift $ s .! #users_registered @?= expUsers

tests :: FilePath -> TestTree
tests wasm_file = testGroup "Tests" $ upgradeGroups $
  [ withoutUpgrade $ idpTest "installs" $ \ _cid ->
    return ()
  , withoutUpgrade $ idpTest "installs and upgrade" $ \ cid ->
    doUpgrade cid
  , withoutUpgrade $ idpTest "register with wrong user fails" $ \cid -> do
    callIDPReject cid dummyUserId #register (device1, invalidPOW)
  , withoutUpgrade $ idpTest "register with bad pow fails" $ \cid -> do
    callIDPReject cid dummyUserId #register (device1, invalidPOW)
  , withoutUpgrade $ idpTest "register with future pow fails" $ \cid -> do
    callIDPReject cid dummyUserId #register (device1, powAt cid (20*60*1000_000_000))
  , withoutUpgrade $ idpTest "register with past pow fails" $ \cid -> do
    setCanisterTimeTo cid (20*60*1000_000_000)
    callIDPReject cid dummyUserId #register (device1, powAt cid 1)
  , withoutUpgrade $ idpTest "register with repeated pow fails" $ \cid -> do
    _ <- callIDP cid webauthID #register (device1, powAt cid 1)
    callIDPReject cid dummyUserId #register (device1, powAt cid 1)
  , withoutUpgrade $ idpTest "get delegation without authorization" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)
    let sessionSK = createSecretKeyEd25519 "hohoho"
    let sessionPK = toPublicKey sessionSK
    let delegationArgs = (user_number, "front.end.com", sessionPK)
    (_, ts) <- callIDP cid webauthID #prepare_delegation delegationArgs
    queryIDPReject cid dummyUserId #get_delegation (addTS delegationArgs ts)

  , withUpgrade $ \should_upgrade -> idpTest "lookup on fresh" $ \cid -> do
    assertStats cid 0
    when should_upgrade $ doUpgrade cid
    assertStats cid 0
    lookupIs cid 123 []

  , withUpgrade $ \should_upgrade -> idpTest "register and lookup" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)
    assertStats cid 1
    when should_upgrade $ doUpgrade cid
    assertStats cid 1
    lookupIs cid user_number [device1]

  , withUpgrade $ \should_upgrade -> idpTest "register and lookup (with credential id)" $ \cid -> do
    user_number <- callIDP cid webauth2ID #register (device2, powAt cid 0)
    when should_upgrade $ doUpgrade cid
    lookupIs cid user_number [device2]

  , withUpgrade $ \should_upgrade -> idpTest "register add lookup" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)
    when should_upgrade $ doUpgrade cid
    callIDP cid webauthID #add (user_number, device2)
    when should_upgrade $ doUpgrade cid
    lookupIs cid user_number [device1, device2]

  , withUpgrade $ \should_upgrade -> idpTest "register and add with wrong user" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)
    when should_upgrade $ doUpgrade cid
    callIDPReject cid webauth2ID #add (user_number, device2)
    lookupIs cid user_number [device1]

  , withUpgrade $ \should_upgrade -> idpTest "get delegation and validate" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)

    let sessionSK = createSecretKeyEd25519 "hohoho"
    let sessionPK = toPublicKey sessionSK
    let delegationArgs = (user_number, "front.end.com", sessionPK)
    -- prepare delegation
    (userPK, ts) <- callIDP cid webauthID #prepare_delegation delegationArgs
    ts <- if should_upgrade
      then do
        doUpgrade cid
        -- after upgrade, no signature is available
        V.IsJust (V.Label :: Label "no_such_delegation") ()
          <- queryIDP cid webauthID #get_delegation (addTS delegationArgs ts)
        -- so request it again
        (userPK', ts') <- callIDP cid webauthID #prepare_delegation delegationArgs
        lift $ userPK' @?= userPK
        return ts'
      else return ts

    V.IsJust (V.Label :: Label "signed_delegation") sd
      <- queryIDP cid webauthID #get_delegation (addTS delegationArgs ts)
    let delegation = sd .! #delegation
    let sig = sd .! #signature
    lift $ delegation .! #pubkey @?= sessionPK

    root_key <- gets $ toPublicKey . secretRootKey
    case verify root_key "ic-request-auth-delegation" userPK (delegationHash delegation) sig of
      Left err -> liftIO $ assertFailure $ T.unpack err
      Right () -> return ()

  , withUpgrade $ \should_upgrade -> idpTest "get delegation with wrong user" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)
    when should_upgrade $ do
      doUpgrade cid

    let sessionSK = createSecretKeyEd25519 "hohoho"
    let sessionPK = toPublicKey sessionSK
    let delegationArgs = (user_number, "front.end.com", sessionPK)
    callIDPReject cid webauth2ID #prepare_delegation delegationArgs

  , withUpgrade $ \should_upgrade -> idpTest "get multiple delegations and validate" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)

    let sessionSK = createSecretKeyEd25519 "hohoho"
    let sessionPK = toPublicKey sessionSK
    let delegationArgs = (user_number, "front.end.com", sessionPK)
    -- request a few delegations
    (userPK, ts1) <- callIDP cid webauthID #prepare_delegation delegationArgs
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts1

    (userPK, ts2) <- callIDP cid webauthID #prepare_delegation delegationArgs
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts1
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts2

    when should_upgrade $ do
      doUpgrade cid

    (userPK, ts3) <- callIDP cid webauthID #prepare_delegation delegationArgs
    unless should_upgrade $ getAndValidate cid sessionPK userPK webauthID delegationArgs ts1
    unless should_upgrade $ getAndValidate cid sessionPK userPK webauthID delegationArgs ts2
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts3

    (userPK, ts4) <- callIDP cid webauthID #prepare_delegation delegationArgs
    unless should_upgrade $ getAndValidate cid sessionPK userPK webauthID delegationArgs ts1
    unless should_upgrade $ getAndValidate cid sessionPK userPK webauthID delegationArgs ts2
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts3
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts4

  , withoutUpgrade $ idpTest "get multiple delegations and expire" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)

    let sessionSK = createSecretKeyEd25519 "hohoho"
    let sessionPK = toPublicKey sessionSK
    let delegationArgs = (user_number, "front.end.com", sessionPK)
    -- request a few delegations
    (userPK, ts1) <- callIDP cid webauthID #prepare_delegation delegationArgs
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts1

    setCanisterTimeTo cid (7*60*1000_000_000)
    (userPK, ts2) <- callIDP cid webauthID #prepare_delegation delegationArgs
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts1
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts2

    setCanisterTimeTo cid (14*60*1000_000_000)
    (userPK, ts3) <- callIDP cid webauthID #prepare_delegation delegationArgs
    getButNotThere cid webauthID delegationArgs ts1
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts2
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts3

    setCanisterTimeTo cid (21*60*1000_000_000)
    (userPK, ts4) <- callIDP cid webauthID #prepare_delegation delegationArgs
    getButNotThere cid webauthID delegationArgs ts1
    getButNotThere cid webauthID delegationArgs ts2
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts3
    getAndValidate cid sessionPK userPK webauthID delegationArgs ts4

  , withUpgrade $ \should_upgrade -> idpTest "user identities differ" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)

    let sessionSK = createSecretKeyEd25519 "hohoho"
    let sessionPK = toPublicKey sessionSK
    let delegationArgs1 = (user_number, "front.end.com", sessionPK)
    (user1PK, _exp) <- callIDP cid webauthID #prepare_delegation delegationArgs1

    when should_upgrade $ do
      doUpgrade cid

    let delegationArgs2 = (user_number, "other-front.end.com", sessionPK)
    (user2PK, _exp) <- callIDP cid webauthID #prepare_delegation delegationArgs2

    when (user1PK == user2PK) $
      lift $ assertFailure "User identities coincide for different frontends"

  , withUpgrade $ \should_upgrade -> idpTest "remove()" $ \cid -> do
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)
    lookupIs cid user_number [device1]
    callIDP cid webauthID #add (user_number, device2)
    lookupIs cid user_number [device1, device2]
    -- NB: removing device that is signing this:
    callIDP cid webauthID #remove (user_number, webauthPK)
    lookupIs cid user_number [device2]
    when should_upgrade $ doUpgrade cid
    lookupIs cid user_number [device2]
    callIDP cid webauth2ID #remove (user_number, webauth2PK)
    when should_upgrade $ doUpgrade cid
    lookupIs cid user_number []
    user_number2 <- callIDP cid webauthID #register (device1, powAt cid 1)
    when should_upgrade $ doUpgrade cid
    when (user_number == user_number2) $
      lift $ assertFailure "User number re-used"

  , withUpgrade $ \should_upgrade -> idpTestWithInit "init range" (100, 103) $ \cid -> do
    s <- queryIDP cid dummyUserId #stats ()
    lift $ s .! #assigned_user_number_range .! #_0_ @?= 100
    lift $ s .! #assigned_user_number_range .! #_1_ @?= 103

    assertStats cid 0
    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)
    liftIO $ user_number @?= 100
    assertStats cid 1
    user_number <- callIDP cid webauthID #register (device1, powAt cid 1)
    liftIO $ user_number @?= 101
    assertStats cid 2

    when should_upgrade $ doUpgrade cid
    s <- queryIDP cid dummyUserId #stats ()
    lift $ s .! #assigned_user_number_range .! #_0_ @?= 100
    lift $ s .! #assigned_user_number_range .! #_1_ @?= 103

    user_number <- callIDP cid webauthID #register (device1, powAt cid 0)
    liftIO $ user_number @?= 102
    assertStats cid 3
    callIDPReject cid webauthID #register (device1, powAt cid 0)
    assertStats cid 3

  , withoutUpgrade $ idpTestWithInit "empty init range" (100, 100) $ \cid -> do
    s <- queryIDP cid dummyUserId #stats ()
    lift $ s .! #assigned_user_number_range .! #_0_ @?= 100
    lift $ s .! #assigned_user_number_range .! #_1_ @?= 100
    callIDPReject cid webauthID #register (device1, powAt cid 0)
  ]
  where
    idpTestWithInit name init act = testCase name $ do
      wasm <- BS.readFile wasm_file
      ic <- initialIC
      flip evalStateT ic $ do
        r <- callManagement controllerId #create_canister (#settings .== Nothing)
        let Candid.Principal cid = r .! #canister_id
        callManagement controllerId #install_code $ empty
          .+ #mode .== V.IsJust #install ()
          .+ #canister_id .== Candid.Principal cid
          .+ #wasm_module .== wasm
          .+ #arg .== Candid.encode (toInitCandid init)
        act cid

    idpTest name act = testCase name $ do
      wasm <- BS.readFile wasm_file
      ic <- initialIC
      flip evalStateT ic $ do
        r <- callManagement controllerId #create_canister (#settings .== Nothing)
        let Candid.Principal cid = r .! #canister_id
        callManagement controllerId #install_code $ empty
          .+ #mode .== V.IsJust #install ()
          .+ #canister_id .== Candid.Principal cid
          .+ #wasm_module .== wasm
          .+ #arg .== Candid.encode (Nothing :: Maybe InitCandid) -- default value
        act cid

    withUpgrade act = ([act False], [act True])
    withoutUpgrade act = ([act], [])

    upgradeGroups :: [([TestTree], [TestTree])] -> [TestTree]
    upgradeGroups ts =
      [ testGroup "without upgrade" (concat without)
      , testGroup "with upgrade" (concat with)
      ]
      where (without, with) = unzip ts

    doUpgrade cid = do
      wasm <- liftIO $ BS.readFile wasm_file
      callManagement controllerId #install_code $ empty
        .+ #mode .== V.IsJust #upgrade ()
        .+ #canister_id .== Candid.Principal cid
        .+ #wasm_module .== wasm
        .+ #arg .== Candid.encode ()

asHex :: Blob -> String
asHex = T.unpack . H.encodeHex . BS.toStrict

-- Configuration: The Wasm file to test
newtype WasmOption = WasmOption String

instance IsOption WasmOption where
  defaultValue = WasmOption "../target/wasm32-unknown-unknown/release/idp_service.wasm"
  parseValue = Just . WasmOption
  optionName = return "wasm"
  optionHelp = return "webassembly module of the identity provider"
  optionCLParser = mkOptionCLParser (metavar "WASM")


wasmOption :: OptionDescription
wasmOption = Option (Proxy :: Proxy WasmOption)

main :: IO ()
main = defaultMainWithIngredients ingredients $ askOption $ \(WasmOption wasm) -> tests wasm
  where
    ingredients =
      [ rerunningTests
        [ listingTests
        , includingOptions [wasmOption]
        , antXMLRunner `composeReporters` htmlRunner `composeReporters` consoleTestReporter
        ]
      ]
