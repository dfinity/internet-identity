{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- |
This module defines ToJSON instances of the IC state.

We put them into their own module, despite the usual advise against orphan
instances, to emphasize that these are there just for debugging purposes using
`ic-ref`.

(Why JSON? Because Browsers render them nicely in a interactive display where
you can open and collapse subcomponents – much easier to get this feature this
way, compared to writing custom HTML output.)
-}

module IC.Debug.JSON () where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

import qualified Data.ByteString.Lazy as BS
import qualified Wasm.Syntax.Values as W
import qualified Wasm.Syntax.AST as W
import qualified Text.Hex as H
import qualified Data.Text as T
import Control.Monad.Random.Lazy

import IC.Types
import IC.Wasm.Winter.Persist
import IC.Purify
import IC.Canister.Snapshot
import IC.Canister
import IC.Ref
import IC.Crypto

customOptions :: Options
customOptions = defaultOptions
    { sumEncoding = ObjectWithSingleField
    }

instance ToJSON W.Value where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance ToJSON BS.ByteString where
    toJSON = String . H.encodeHex . BS.toStrict

instance ToJSONKey BS.ByteString where
    toJSONKey = toJSONKeyText (H.encodeHex . BS.toStrict)

instance ToJSON (W.Module f) where
    toJSON = placeholder "(module)"

instance ToJSON (Replay i) where
    toJSON = placeholder "(replay)"

placeholder :: String -> a -> Value
placeholder s = const (String (T.pack s))

deriving instance Generic Timestamp
instance ToJSON Timestamp where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic Responded
instance ToJSON Responded where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic RejectCode
instance ToJSON RejectCode where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic Response
instance ToJSON Response where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic WasmClosure
instance ToJSON WasmClosure where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic Callback
instance ToJSON Callback where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic MethodCall
instance ToJSON MethodCall where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic PInstance
instance ToJSON PInstance where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic PModuleInst
instance ToJSON PModuleInst where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic (Snapshot a)
instance ToJSON a => ToJSON (Snapshot a) where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic CanisterSnapshot
instance ToJSON CanisterSnapshot where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic IC
instance ToJSON IC where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic CallContext
instance ToJSON CallContext where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic Message
instance ToJSON Message where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic RequestStatus
instance ToJSON RequestStatus where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic CallResponse
instance ToJSON CallResponse where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions


deriving instance Generic CallRequest
instance ToJSON CallRequest where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic RunStatus
instance ToJSON RunStatus where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic CanState
instance ToJSON CanState where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic CanisterContent
instance ToJSON CanisterContent where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic CallOrigin
instance ToJSON CallOrigin where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

deriving instance Generic EntryPoint
instance ToJSON EntryPoint where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance ToJSON CanisterModule where
    toJSON = placeholder "(CanisterModule)"

instance ToJSON EntityId where
  toJSON = toJSON . prettyID

instance ToJSONKey EntityId where
  toJSONKey = contramapToJSONKeyFunction prettyID toJSONKey

instance ToJSON StdGen where
  toJSON = toJSON . show

instance ToJSON SecretKey where
    toJSON = placeholder "(secret key)"
