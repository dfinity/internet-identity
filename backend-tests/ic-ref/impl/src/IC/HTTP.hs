{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module IC.HTTP where

import Network.Wai
import Control.Concurrent (forkIO)
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Builder (stringUtf8)
import Control.Monad.State
import Control.Monad.Except
import Data.Aeson as JSON
import Codec.Candid (Principal(..), parsePrincipal)

import IC.Types
import IC.Ref
import IC.HTTP.Status
import IC.HTTP.CBOR
import IC.HTTP.GenR
import IC.HTTP.Request
import IC.HTTP.RequestId
import IC.Debug.JSON ()
import IC.Serialise ()
import IC.StateFile
import IC.Crypto

withApp :: Maybe FilePath -> (Application -> IO a) -> IO a
withApp backingFile action =
    withStore initialIC backingFile (action . handle)

handle :: Store IC -> Application
handle store req respond = case (requestMethod req, pathInfo req) of
    ("GET", []) -> peekStore store >>= json status200
    ("GET", ["api","v1",_]) -> noV1 req
    ("GET", ["api","v2","status"]) -> do
        r <- peekIC $ gets IC.HTTP.Status.r
        cbor status200 r
    ("POST", ["api","v2","canister",textual_ecid,verb]) ->
        case parsePrincipal textual_ecid of
            Left err -> invalidRequest $ "cannot parse effective canister id: " <> T.pack err
            Right (Principal ecid) -> do
              root_key <- peekIC $ gets $ toPublicKey . secretRootKey
              case verb of
                "call" -> withSignedCBOR root_key $ \(gr, ev) -> case callRequest gr of
                    Left err -> invalidRequest err
                    Right cr -> runIC $ do
                        t <- lift getTimestamp
                        runExceptT (authCallRequest t (EntityId ecid) ev cr) >>= \case
                            Left err ->
                                lift $ invalidRequest err
                            Right () -> do
                                submitRequest (requestId gr) cr
                                lift $ empty status202
                "query" -> withSignedCBOR root_key $ \(gr, ev) -> case queryRequest gr of
                    Left err -> invalidRequest err
                    Right qr -> peekIC $ do
                        t <- lift getTimestamp
                        runExceptT (authQueryRequest t (EntityId ecid) ev qr) >>= \case
                            Left err ->
                                lift $ invalidRequest err
                            Right () -> do
                                t <- lift getTimestamp
                                r <- handleQuery t qr
                                lift $ cbor status200 (IC.HTTP.Request.response r)
                "read_state" -> withSignedCBOR root_key $ \(gr, ev) -> case readStateRequest gr of
                    Left err -> invalidRequest err
                    Right rsr -> peekIC $ do
                        t <- lift getTimestamp
                        runExceptT (authReadStateRequest t (EntityId ecid) ev rsr) >>= \case
                            Left err ->
                                lift $ invalidRequest err
                            Right () -> do
                                t <- lift getTimestamp
                                r <- handleReadState t rsr
                                lift $ cbor status200 (IC.HTTP.Request.response r)
                _ -> notFound req
    _ -> notFound req
  where
    runIC :: StateT IC IO a -> IO a
    runIC a = do
      x <- modifyStore store a
      -- begin processing in the background (it is important that
      -- this thread returns, else warp is blocked somehow)
      void $ forkIO loopIC
      return x

    -- Not atomic, reads most recent state
    peekIC :: StateT IC IO a -> IO a
    peekIC a = peekStore store >>= evalStateT a

    loopIC :: IO ()
    loopIC = modifyStore store runStep >>= \case
        True -> loopIC
        False -> return ()

    cbor status gr = respond $ responseBuilder
        status
        [ (hContentType, "application/cbor") ]
        (IC.HTTP.CBOR.encode gr)

    json status x = respond $ responseBuilder
        status
        [ (hContentType, "application/json") ]
        (JSON.fromEncoding $ JSON.toEncoding x)

    plain status x = respond $ responseBuilder
        status
        [ (hContentType, "text/plain") ]
        x

    empty status = plain status mempty

    invalidRequest msg = do
        when False $ print (T.unpack msg)
        -- ^ When testing against dfx, and until it prints error messages
        -- this can be enabled
        plain status400 (T.encodeUtf8Builder msg)

    notFound req = plain status404 $ stringUtf8 $
        "ic-ref does not know how to handle a " ++ show (requestMethod req) ++
        " request to " ++ show (rawPathInfo req)

    noV1 req = plain status404 $ stringUtf8 $
        "ic-ref no longer supports the v1 HTTP API at " ++ show (rawPathInfo req)

    withCBOR k = case lookup hContentType (requestHeaders req) of
        Just "application/cbor" -> do
            body <- strictRequestBody req
            case IC.HTTP.CBOR.decode body of
                Left err -> invalidRequest err
                Right gr -> k gr
        _ -> invalidRequest "Expected application/cbor request"

    withSignedCBOR :: Blob -> ((GenR, EnvValidity) -> IO ResponseReceived) -> IO ResponseReceived
    withSignedCBOR root_key k = withCBOR $ either invalidRequest k . stripEnvelope root_key
