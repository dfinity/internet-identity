{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE FlexibleContexts #-}
{- | Parses/produces generic requests -}
module IC.HTTP.Request where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Lazy as HM
import Control.Monad.Except
import Control.Monad.Writer
import Data.Foldable
import Data.Time.Clock.POSIX
import Data.Maybe
import Data.Bifunctor

import IC.Types
import IC.Crypto
import IC.Ref (CallRequest(..), QueryRequest(..), ReadStateRequest(..),
  ReqResponse(..), CallResponse(..))
import IC.Id.Forms hiding (Blob)
import IC.Certificate.CBOR
import IC.HTTP.RequestId
import IC.HTTP.GenR
import IC.HTTP.GenR.Parse

dummyUserId :: EntityId
dummyUserId = EntityId $ BS.pack [0xCA, 0xFF, 0xEE]

stripEnvelope :: Blob -> GenR -> Either T.Text (GenR, EnvValidity)
stripEnvelope root_key gr = runWriterT $ flip record gr $ do
    content <- field anyType "content"
    pk <- optionalField blob "sender_pubkey"
    sig <- optionalField blob "sender_sig"
    checkExpiry content
    case (pk, sig) of
        (Just pk, Just sig) -> do
            tell $ validFor $ \(EntityId id) ->
              unless (isSelfAuthenticatingId pk id) $
                throwError "Public key not authorized to sign for user"

            delegations <- optionalField (listOf delegationField) "sender_delegation"
            pk' <- checkDelegations pk (fromMaybe [] delegations)

            let rid = requestId content
            lift $ lift $
                first (<> "\nExpected request id: " <> T.pack (prettyBlob rid)
                       <> "\nPublic Key:          " <> T.pack (prettyBlob pk')
                       <> "\nSignature:           " <> T.pack (prettyBlob sig)) $
                verify root_key "ic-request" pk' rid sig
        (Nothing, Nothing) ->
            tell $ validFor $ \(EntityId id) ->
              unless (isAnonymousId id) $
                throwError "Missing signature, but user is not the anonymous user"
        _ -> throwError "Need to set either both or none of sender_pubkey and sender_sig"
    return content

    where
      checkDelegations pk [] = return pk
      checkDelegations pk ((pk', Timestamp expiry, targets, hash, sig):ds) = do
          lift $ lift $ verify root_key "ic-request-auth-delegation" pk hash sig
          tell $ validWhen $ \(Timestamp t) ->
              unless (expiry > t) $
                throwError $ "Delegation expiry is " <> T.pack (show ((t - expiry)`div`1000_000_000)) <> " seconds in the past"
          for_ targets $ \ts ->
            tell $ validWhere $ \c ->
              unless (c `elem` ts) $
                throwError "Delegation does not apply to this canister"
          checkDelegations pk' ds


      checkExpiry :: GenR -> RecordM (WriterT EnvValidity (Either T.Text)) ()
      checkExpiry (GRec hm)
          | Just (GNat expiry) <- HM.lookup "ingress_expiry" hm =
            tell $ validWhen $ \(Timestamp t) -> do
              -- Here we check that the expiry field is not in the past and not
              -- too far in the future
              unless (expiry > t) $
                  throwError $ "Expiry is " <> T.pack (show ((t - expiry)`div`1000_000_000)) <> " seconds in the past"
              unless (expiry < t + max_future) $
                  throwError $ "Expiry is " <> T.pack (show ((expiry - t)`div`1000_000_000)) <> " seconds in the future"
        where
          -- max expiry time is 5 minutes
          max_future = 5*60*1000_000_000
      checkExpiry _ = throwError "No ingress_expiry field found"


type DelegationHash = Blob
delegationField :: Field (PublicKey, Timestamp, Maybe [EntityId], DelegationHash, Blob)
delegationField = record $ do
    delegation <- field anyType "delegation"
    (pk, ts, targets) <- lift $ flip record delegation $ do
            pk <- field blob "pubkey"
            ts <- Timestamp <$> field nat "expiration"
            targets <- optionalField (listOf entitiyId) "targets"
            return (pk, ts, targets)
    sig <- field blob "signature"
    return (pk, ts, targets, requestId delegation, sig)

getTimestamp :: IO Timestamp
getTimestamp = do
    t <- getPOSIXTime
    return $ Timestamp $ round (t * 1000_000_000)

-- Parsing requests
callRequest :: GenR -> Either T.Text CallRequest
callRequest = record $ do
    t <- field text "request_type"
    unless (t == "call") $
        throwError $ "Expected request_type to be \"call\", got \"" <> t <> "\""
    _ <- optionalField blob "nonce"
    _ <- field nat "ingress_expiry"
    cid <- field entitiyId "canister_id"
    sender <- field entitiyId "sender"
    method_name <- field text "method_name"
    arg <- field blob "arg"
    return $ CallRequest cid sender (T.unpack method_name) arg

queryRequest :: GenR -> Either T.Text QueryRequest
queryRequest = record $ do
    t <- field text "request_type"
    unless (t == "query") $
        throwError $ "Expected request_type to be \"query\", got \"" <> t <> "\""
    _ <- optionalField blob "nonce"
    _ <- field nat "ingress_expiry"
    cid <- field entitiyId "canister_id"
    sender <- field entitiyId "sender"
    method_name <- field text "method_name"
    arg <- field blob "arg"
    return $ QueryRequest cid sender (T.unpack method_name) arg

readStateRequest :: GenR -> Either T.Text ReadStateRequest
readStateRequest = record $ do
    t <- field text "request_type"
    unless (t == "read_state") $
        throwError $ "Expected request_type to be \"read_state\", got \"" <> t <> "\""
    _ <- optionalField blob "nonce"
    _ <- field nat "ingress_expiry"
    sender <- field entitiyId "sender"
    paths <- field (listOf (listOf blob)) "paths"
    return $ ReadStateRequest sender paths

entitiyId :: Field EntityId
entitiyId = fmap EntityId <$> blob

-- Printing responses
response :: ReqResponse -> GenR
response (QueryResponse (Rejected (c, s))) = rec
    [ "status" =: GText "rejected"
    , "reject_code" =: GNat (fromIntegral (rejectCode c))
    , "reject_message" =: GText (T.pack s)
    ]
response (QueryResponse (Replied blob)) = rec
    [ "status" =: GText "replied"
    , "reply" =: rec [ "arg" =: GBlob blob ]
    ]
response (ReadStateResponse cert) = rec
    [ "certificate" =: GBlob (encodeCert cert)
    ]
