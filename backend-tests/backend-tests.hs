{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Options.Applicative hiding (empty)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Hex as H
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Vector as V
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.CaseInsensitive  ( CI )
import qualified Data.CaseInsensitive as CI
import Control.Monad.Trans
import Control.Monad.Trans.State
import Codec.CBOR.Term
import Codec.CBOR.Read
import Data.Proxy
import Data.Bifunctor
import Data.Word
import Control.Monad.Random.Lazy
import Data.Time.Clock.POSIX
import Text.Printf
import Text.Regex.TDFA ((=~), (=~~))

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Runners.AntXML
import Test.Tasty.Options
import Test.Tasty.Runners.Html

import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Row (empty, (.==), (.+), type (.!), (.!), Label, AllUniqueLabels, Var)
import Codec.Candid (Principal(..))
import qualified Codec.Candid as Candid
import qualified Data.Row.Variants as V

import IC.Types hiding (PublicKey, Timestamp)
import qualified IC.Types
import IC.Ref
import IC.Management
import IC.Hash
import IC.Crypto
import IC.Id.Forms hiding (Blob)
import IC.HTTP.GenR
import IC.HTTP.RequestId
import IC.Certificate hiding (Delegation)
import IC.Certificate.CBOR
import IC.Certificate.Validate
import IC.HashTree hiding (Blob, Hash, Label)
import IC.HashTree.CBOR

import Prometheus hiding (Timestamp)

type IIInterface m = [Candid.candidFile|../src/internet_identity/internet_identity.did|]

-- Pulls in all type definitions as Haskell type aliases
[Candid.candidDefsFile|../src/internet_identity/internet_identity.did|]

httpGet :: String -> HttpRequest
httpGet url = #method .== T.pack "GET"
  .+ #url .== T.pack url
  .+ #headers .== V.empty
  .+ #body .== ""

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
    t <- getTimestamp
    QueryResponse r <- handleQuery t r
    return r

getTimestamp :: M IC.Types.Timestamp
getTimestamp = lift $ do
    t <- getPOSIXTime
    return $ IC.Types.Timestamp $ round (t * 1000_000_000)

mkRequestId :: IO RequestID
mkRequestId = BS.toLazyByteString . BS.word64BE <$> randomIO

setCanisterTimeTo :: Blob -> IC.Types.Timestamp -> M ()
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

queryII :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IIInterface IO .! s) =>
  Candid.CandidArg a =>
  Candid.CandidArg b =>
  BS.ByteString -> EntityId -> Label s -> a -> M b
queryII cid user_id l x = do
  r <- submitQuery $ QueryRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected (_code, msg) -> lift $ assertFailure $ "II query got rejected:\n" <> msg
    Replied b -> case Candid.decode b of
      Left err -> lift $ assertFailure err
      Right y -> return y

queryIIReject :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IIInterface IO .! s) =>
  Candid.CandidArg a =>
  BS.ByteString -> EntityId -> Label s -> a -> M ()
queryIIReject cid user_id l x = do
  r <- submitQuery $ QueryRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected _ -> return ()
    Replied _ -> lift $ assertFailure "queryIIReject: Unexpected reply"

queryIIRejectWith :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IIInterface IO .! s) =>
  Candid.CandidArg a =>
  BS.ByteString -> EntityId -> Label s -> a -> String -> M ()
queryIIRejectWith cid user_id l x expectedMessagePattern = do
  r <- submitQuery $ QueryRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected (_code, msg) ->
      if not (msg =~ expectedMessagePattern)
      then liftIO $ assertFailure $ printf "expected error matching %s, got: %s" (show expectedMessagePattern) (show msg)
      else return ()
    Replied _ -> lift $ assertFailure "queryIIRejectWith: Unexpected reply"

callII :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IIInterface IO .! s) =>
  Candid.CandidArg a =>
  Candid.CandidArg b =>
  BS.ByteString -> EntityId -> Label s -> a -> M b
callII cid user_id l x = do
  r <- submitAndRun $ CallRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected (_code, msg) -> lift $ assertFailure $ "II call got rejected:\n" <> msg
    Replied b -> case Candid.decode b of
      Left err -> lift $ assertFailure err
      Right y -> return y

callIIReject :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IIInterface IO .! s) =>
  Candid.CandidArg a =>
  BS.ByteString -> EntityId -> Label s -> a -> M ()
callIIReject cid user_id l x = do
  r <- submitAndRun $
    CallRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected _ -> return ()
    Replied _ -> lift $ assertFailure "callIIReject: Unexpected reply"

callIIRejectWith :: forall s a b.
  HasCallStack =>
  KnownSymbol s =>
  (a -> IO b) ~ (IIInterface IO .! s) =>
  Candid.CandidArg a =>
  BS.ByteString -> EntityId -> Label s -> a -> String  -> M ()
callIIRejectWith cid user_id l x expectedMessagePattern = do
  r <- submitAndRun $
    CallRequest (EntityId cid) user_id (symbolVal l) (Candid.encode x)
  case r of
    Rejected (_code, msg) ->
      if not (msg =~ expectedMessagePattern)
      then liftIO $ assertFailure $ printf "expected error matching %s, got: %s" (show expectedMessagePattern) (show msg)
      else return ()
    Replied _ -> lift $ assertFailure "callIIRejectWith: Unexpected reply"


-- Some common devices
-- NOTE: we write the actual key content here, as opposed to generating it
-- (e.g. with 'createSecretKeyWebAuthnECDSA'). This ensures the key contents
-- are stable.
-- See also: https://github.com/dfinity/ic-hs/issues/59
webauth1PK :: PublicKey
webauth1PK = "0^0\f\ACK\n+\ACK\SOH\EOT\SOH\131\184C\SOH\SOH\ETXN\NUL\165\SOH\STX\ETX& \SOH!X lR\190\173]\245, \138\155\FS{\224\166\bGW>[\228\172O\224\142\164\128\&6\208\186\GS*\207\"X \179=\174\184;\201\199}\138\215b\253h\227\234\176\134\132\228c\196\147Q\179\171*\DC4\164\NUL\DC3\131\135"
webauth1ID :: EntityId
webauth1ID = EntityId $ mkSelfAuthenticatingId webauth1PK
device1 :: DeviceData
device1 = empty
    .+ #alias .== "device1"
    .+ #pubkey .== webauth1PK
    .+ #credential_id .== Nothing
    .+ #purpose .== enum #authentication
    .+ #protection .== enum #unprotected
    .+ #key_type .== enum #cross_platform

webauth2SK :: SecretKey
webauth2SK = createSecretKeyWebAuthnRSA "foobar2"
-- The content here doesn't matter as long as it's different from webauth1PK
webauth2PK = toPublicKey webauth2SK
webauth2PK :: PublicKey
webauth2ID :: EntityId
webauth2ID = EntityId $ mkSelfAuthenticatingId webauth2PK
device2 :: DeviceData
device2 = empty
    .+ #alias .== "device2"
    .+ #pubkey .== webauth2PK
    .+ #credential_id .== Just "foobar"
    .+ #purpose .== enum #authentication
    .+ #protection .== enum #unprotected
    .+ #key_type .== enum #platform

webauth3SK :: SecretKey
webauth3SK = createSecretKeyWebAuthnRSA "foobar3"
webauth3PK = toPublicKey webauth3SK
webauth3PK :: PublicKey
webauth3ID :: EntityId
webauth3ID = EntityId $ mkSelfAuthenticatingId webauth3PK

device3 :: DeviceData
device3 = empty
    .+ #alias .== "device3"
    .+ #pubkey .== webauth3PK
    .+ #credential_id .== Just "foobar"
    .+ #purpose .== enum #authentication
    .+ #key_type .== enum #seed_phrase
    .+ #protection .== enum #protected

device4 :: DeviceData
device4 = empty
    .+ #alias .== "device4"
    .+ #pubkey .== webauth1PK
    .+ #credential_id .== Just "foobar"
    .+ #purpose .== enum #authentication
    .+ #key_type .== enum #seed_phrase
    .+ #protection .== enum #protected

anonymousID :: EntityId
anonymousID = EntityId "\x04"

-- Check that the user has the following device data
lookupIs
    :: Blob -- ^ canister ID
    -> Word64 -- ^ user (anchor) number
    -> [DeviceData] -> M ()
lookupIs cid user_number ds = do
  r <- queryII cid dummyUserId #lookup user_number
  liftIO $ r @?= V.fromList ds

addTS :: (a, b, c, d) -> e -> (a, b, c, e)
addTS (a,b,c, _) ts = (a,b,c,ts)

assertStats :: HasCallStack => Blob -> Word64 -> M()
assertStats cid expUsers = do
  s <- queryII cid dummyUserId #stats ()
  lift $ s .! #users_registered @?= expUsers

assertVariant :: (HasCallStack, KnownSymbol l, V.Forall r Show) => Label l -> V.Var r -> M ()
assertVariant label var = case V.view label var of
  Just _ -> return ()
  Nothing -> liftIO $ assertFailure $ printf "expected variant %s, got: %s" (show label) (show var)

mustGetUserNumber :: HasCallStack => RegisterResponse -> M Word64
mustGetUserNumber response = case V.view #registered response of
  Just r -> return (r .! #user_number)
  Nothing -> liftIO $ assertFailure $ "expected to get 'registered' response, got " ++ show response

assertRightS :: MonadIO m  => Either String a -> m a
assertRightS (Left e) = liftIO $ assertFailure e
assertRightS (Right x) = pure x

assertRightT :: MonadIO m  => Either T.Text a -> m a
assertRightT (Left e) = liftIO $ assertFailure (T.unpack e)
assertRightT (Right x) = pure x

decodeTree :: BS.ByteString -> Either T.Text HashTree
decodeTree s =
    first (\(DeserialiseFailure _ s) -> "CBOR decoding failure: " <> T.pack s)
        (deserialiseFromBytes decodeTerm s)
    >>= begin
  where
    begin (leftOver, _)
      | not (BS.null leftOver) = Left $ "Left-over bytes: " <> T.pack (shorten 20 (show leftOver))
    begin (_, TTagged 55799 t) = parseHashTree t
    begin _ = Left "Expected CBOR request to begin with tag 55799"

-- | The actual tests.
tests :: FilePath -> TestTree
tests wasm_file = testGroup "Tests" $ upgradeGroups $
  [ withoutUpgrade $ iiTest "installs" $ \ _cid ->
    return ()
  , withoutUpgrade $ iiTest "installs and upgrade" $ \ cid ->
    doUpgrade cid

  , withUpgrade $ \should_upgrade -> iiTest "lookup on fresh" $ \cid -> do
    assertStats cid 0
    when should_upgrade $ doUpgrade cid
    assertStats cid 0
    lookupIs cid 123 []
  ]
  where
    withIC act = do
      ic <- initialIC
      evalStateT act ic

    iiTestWithInit name (l, u) act = testCase name $ withIC $ do
      wasm <- lift $ BS.readFile wasm_file
      r <- callManagement controllerId #create_canister (#settings .== Nothing)
      let Candid.Principal cid = r .! #canister_id
      callManagement controllerId #install_code $ empty
        .+ #mode .== enum #install
        .+ #canister_id .== Candid.Principal cid
        .+ #wasm_module .== wasm
        .+ #arg .== Candid.encode (Just (#assigned_user_number_range .== (l :: Word64, u :: Word64)))
      act cid

    iiTest name act = testCase name $ withIC $ do
      wasm <- lift $ BS.readFile wasm_file
      r <- callManagement controllerId #create_canister (#settings .== Nothing)
      let Candid.Principal cid = r .! #canister_id
      callManagement controllerId #install_code $ empty
        .+ #mode .== V.IsJust #install ()
        .+ #canister_id .== Candid.Principal cid
        .+ #wasm_module .== wasm
        .+ #arg .== Candid.encode (Nothing :: Maybe InternetIdentityInit) -- default value
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

    getChallenge cid webauthID = do
      challenge <- callII cid webauthID #create_challenge ()
      pure $ #key .== challenge .! #challenge_key .+ #chars .== T.pack "a"

    -- Go through a challenge request/registration flow for this device.
    -- NOTE: this (dummily) solves the challenge with the string "a", which is
    -- returned by the backend when compiled with II_DUMMY_CAPTCHA.
    register cid webauthID device =
      getChallenge cid webauthID >>= callII cid webauthID #register . (device,)

asHex :: Blob -> String
asHex = T.unpack . H.encodeHex . BS.toStrict

fromPrincipal :: T.Text -> Blob
fromPrincipal s = cid
  where Right (Candid.Principal cid) = Candid.parsePrincipal s

enum :: (AllUniqueLabels r, KnownSymbol l, (r .! l) ~ ()) => Label l -> Var r
enum l = V.IsJust l ()

-- Configuration: The Wasm file to test
newtype WasmOption = WasmOption String

instance IsOption WasmOption where
  defaultValue = WasmOption "../internet_identity.wasm"
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
