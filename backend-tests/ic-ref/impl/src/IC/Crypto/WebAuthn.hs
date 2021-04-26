{-|
This module implements WebAuthN crypto. WebauthN is a big mess, involving
nesting of CBOR, DER and JSON…
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
module IC.Crypto.WebAuthn
 ( init
 , SecretKey
 , createKey
 , toPublicKey
 , sign
 , verify
 ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Bifunctor
import Control.Monad
import Data.Hashable
import Data.ByteString.Base64.URL.Lazy as Base64
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified IC.HTTP.CBOR as CBOR
import Codec.CBOR.Term
import Codec.CBOR.Write (toLazyByteString)
import IC.CBOR.Parser
import qualified Data.Map as M
import IC.HTTP.GenR.Parse
import IC.Hash
import Control.Monad.Except
import qualified Crypto.PubKey.ECC.ECDSA as EC
import qualified Crypto.PubKey.ECC.Generate as EC
import qualified Crypto.PubKey.ECC.Types as EC
import qualified Crypto.Number.Serialize as EC
import Crypto.Hash.Algorithms (SHA256(..))
import Data.ASN1.Types
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import IC.Crypto.DER.Decode

parseSig :: BS.ByteString -> Either T.Text (BS.ByteString, BS.ByteString, BS.ByteString)
parseSig = CBOR.decode >=> record do
      ad <- field blob "authenticator_data"
      cdj <- BS.fromStrict . T.encodeUtf8 <$> field text "client_data_json"
      sig <- field blob "signature"
      return (ad, cdj, sig)

genSig :: (BS.ByteString, BS.ByteString, BS.ByteString) -> BS.ByteString
genSig (ad, cdj, sig) =
  toLazyByteString $ encodeTerm $ TTagged 55799 $ TMap
    [ (TString "authenticator_data", TBytes (BS.toStrict ad))
    , (TString "client_data_json", TString (T.decodeUtf8 (BS.toStrict cdj)))
    , (TString "signature", TBytes (BS.toStrict sig))
    ]

parseClientDataJson :: BS.ByteString -> Either T.Text BS.ByteString
parseClientDataJson blob = first T.pack $
    JSON.eitherDecode blob >>= JSON.parseEither p
  where
    p = JSON.withObject "clientData" $ \o -> do
      x <- o JSON..: "challenge"
      either JSON.parseFail return $ Base64.decodeUnpadded (BS.pack x)

genClientDataJson :: BS.ByteString -> BS.ByteString
genClientDataJson challenge = JSON.encode $ JSON.Object $
    "challenge" JSON..= BS.unpack (Base64.encodeUnpadded challenge)
    <> "type" JSON..= ("webauthn.get" :: T.Text)
    <> "origin" JSON..= ("ic-ref-test" :: T.Text)

parseCOSEKey :: BS.ByteString -> Either T.Text EC.PublicKey
parseCOSEKey s = do
    kv <- decodeWithoutTag s >>= parseMap "COSE key"
    m <- M.fromList <$> mapM keyVal kv
    let field n = case M.lookup n m of
            Just x -> return x
            Nothing -> throwError $ "COSE: missing entry " <> T.pack (show n)
    let intField n = field n >>= \case
            TInt i -> pure i
            _ -> throwError $ "COSE field " <> T.pack (show n) <> " not an int"
    let bytesField n = field n >>= \case
            TBytes b -> pure b
            _ -> throwError $ "COSE field " <> T.pack (show n) <> " not bytes"

    ty <- intField 1
    unless (ty == 2) $
        throwError "COSE: Only key type 2 (EC2) supported"
    ty <- intField 3
    unless (ty == -7) $
        throwError "COSE: Only type -7 (ECDSA) supported"
    crv <- intField (-1)
    unless (crv == 1) $
        throwError $ "parsePublicKey: unknown curve: " <> T.pack (show crv)
    xb <- bytesField (-2)
    yb <- bytesField (-3)
    let x = EC.os2ip xb
    let y = EC.os2ip yb
    return $ EC.PublicKey curve (EC.Point x y)
  where
    keyVal (TInt k,v) = pure (fromIntegral k,v)
    keyVal (TInteger k,v) = pure (k,v)
    keyVal _ = throwError "Non-integer key in CBOR map"

genCOSEKey :: EC.PublicKey -> BS.ByteString
genCOSEKey (EC.PublicKey _curve (EC.Point x y)) =
  toLazyByteString $ encodeTerm $ TMap
    [ (TInt 1, TInt 2)
    , (TInt 3, TInt (-7))
    , (TInt (-1), TInt 1)
    , (TInt (-2), TBytes (EC.i2ospOf_ 32 x))
    , (TInt (-3), TBytes (EC.i2ospOf_ 32 y))
    ]
genCOSEKey (EC.PublicKey _ EC.PointO) = error "genCOSEKey: Point at infinity"



parseCOSESig :: BS.ByteString -> Either T.Text EC.Signature
parseCOSESig s =
  first T.pack (safeDecode s) >>= \case
    [Start Sequence,IntVal r,IntVal s,End Sequence] -> pure $ EC.Signature r s
    a -> throwError $ "Unexpected DER encoding for COSE sig: " <> T.pack (show a)

genCOSESig :: EC.Signature -> BS.ByteString
genCOSESig (EC.Signature r s) = encodeASN1 DER
    [Start Sequence,IntVal r,IntVal s,End Sequence]

data SecretKey = SecretKey EC.PrivateKey EC.PublicKey
  deriving Show

curve :: EC.Curve
curve = EC.getCurveByName EC.SEC_p256r1

createKey :: BS.ByteString -> SecretKey
createKey seed =
    SecretKey (EC.PrivateKey curve d) (EC.PublicKey curve q)
  where
    n = EC.ecc_n $ EC.common_curve curve
    d = fromIntegral (hash seed) `mod` (n-2) + 1
    q = EC.generateQ curve d

toPublicKey :: SecretKey -> BS.ByteString
toPublicKey (SecretKey _ pk) = genCOSEKey pk


sign :: SecretKey -> BS.ByteString -> IO BS.ByteString
sign (SecretKey sk _) msg = do
  let cdj = genClientDataJson msg
  let ad = "arbitrary?"
  sig <- EC.sign sk SHA256 (BS.toStrict (ad <> sha256 cdj))
  return $ genSig (ad, cdj, genCOSESig sig)

verify :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Either T.Text ()
verify pk msg sig = do
    (ad, cdj, sig) <- parseSig sig
    pk <- parseCOSEKey pk
    sig <- parseCOSESig sig
    unless (EC.verify SHA256 pk sig (BS.toStrict $ ad <> sha256 cdj)) $
      throwError "WebAuthn signature verification failed"
    challenge <- parseClientDataJson cdj
    unless (challenge == msg) $
      throwError $ "Wrong challenge. Expected " <> T.pack (show msg) <>
        " got " <> T.pack (show challenge)
