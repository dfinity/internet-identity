{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module IC.Types where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Hex as T hiding (Text)
import Data.Digest.CRC
import Data.Digest.CRC32
import Data.ByteString.Base32
import Data.Int
import Data.List
import Data.List.Split (chunksOf)
import Numeric.Natural
import Control.Monad.Except

type (↦) = M.Map

-- Basic types

type Blob = BS.ByteString
type PublicKey = Blob
newtype EntityId = EntityId { rawEntityId :: Blob }
    deriving (Show, Eq, Ord)

type CanisterId = EntityId
type SubnetId = EntityId
type UserId = EntityId
type MethodName = String
type RequestID = Blob
type Cycles = Natural

prettyBlob :: Blob -> String
prettyBlob b = "0x" ++ T.unpack (T.encodeHex (BS.toStrict b))

prettyID :: EntityId -> String
prettyID (EntityId blob) =
    intercalate "-" (chunksOf 5 (base32 (checkbytes <> blob)))
  where
    CRC32 checksum = digest (BS.toStrict blob)
    checkbytes = BS.toLazyByteString (BS.word32BE checksum)

    base32 = filter (/='=') . T.unpack . T.toLower . encodeBase32 . BS.toStrict


newtype Responded = Responded Bool
  deriving (Show, Eq)

newtype Timestamp = Timestamp Natural
  deriving (Show, Num, Ord, Eq)

data RejectCode
    = RC_SYS_FATAL
    | RC_SYS_TRANSIENT
    | RC_DESTINATION_INVALID
    | RC_CANISTER_REJECT
    | RC_CANISTER_ERROR
  deriving Show

rejectCode :: RejectCode -> Natural
rejectCode RC_SYS_FATAL           = 1
rejectCode RC_SYS_TRANSIENT       = 2
rejectCode RC_DESTINATION_INVALID = 3
rejectCode RC_CANISTER_REJECT     = 4
rejectCode RC_CANISTER_ERROR      = 5


data Response = Reply Blob | Reject (RejectCode, String)
  deriving Show

-- Abstract canisters

-- | This data type contains all read-only data that should be available to the
-- canister almost always
data Status = Running | Stopping | Stopped
data Env = Env
    { env_self :: CanisterId
    , env_time :: Timestamp
    , env_balance :: Cycles
    , env_status :: Status
    , env_certificate :: Maybe Blob
    }

data TrapOr a = Trap String | Return a deriving Functor

data WasmClosure = WasmClosure
  { closure_idx :: Int32
  , closure_env :: Int32
  }
  deriving Show

data Callback = Callback
  { reply_callback :: WasmClosure
  , reject_callback :: WasmClosure
  , cleanup_callback :: Maybe WasmClosure
  }
  deriving Show

data MethodCall = MethodCall
  { call_callee :: CanisterId
  , call_method_name :: MethodName
  , call_arg :: Blob
  , call_callback :: Callback
  , call_transferred_cycles :: Cycles
  }
  deriving Show

type ExistingCanisters = [CanisterId]

-- Canister actions (independent of calls)
newtype CanisterActions = CanisterActions
  { set_certified_data :: Maybe Blob
  }

instance Semigroup CanisterActions where
    ca1 <> ca2 = CanisterActions (set_certified_data ca1 `setter` set_certified_data ca2)
      where
        setter _ (Just x) = Just x
        setter x Nothing = x

noCanisterActions :: CanisterActions
noCanisterActions = CanisterActions Nothing

-- Actions relative to a call context
data CallActions = CallActions
  { ca_new_calls :: [MethodCall]
  , ca_accept :: Cycles
  , ca_response :: Maybe Response
  }

noCallActions :: CallActions
noCallActions = CallActions [] 0 Nothing

type UpdateResult = (CallActions, CanisterActions)

type StableMemory = Blob

-- Semantically relevant information from an envelope
--
--  * When is it valid
--  * Which users can it sign for
--  * Which canisters can it be used at
--
-- All represented as validation functions

type ValidityPred a = forall m. MonadError T.Text m => a -> m ()
data EnvValidity = EnvValidity
    { valid_when  :: ValidityPred Timestamp
    , valid_for   :: ValidityPred EntityId
    , valid_where :: ValidityPred EntityId
    }

instance Semigroup EnvValidity where
    ed1 <> ed2 = EnvValidity
        { valid_when  = valid_when  ed1 >>> valid_when  ed2
        , valid_for   = valid_for   ed1 >>> valid_for   ed2
        , valid_where = valid_where ed1 >>> valid_where ed2
        } where a >>> b = \x -> a x >> b x
instance Monoid EnvValidity where
    mempty = EnvValidity x x x
      where
        x :: ValidityPred a
        x = const (return ())

validWhen :: ValidityPred Timestamp -> EnvValidity
validWhen valid_when = mempty { valid_when }

validFor :: ValidityPred EntityId -> EnvValidity
validFor valid_for = mempty { valid_for }

validWhere :: ValidityPred EntityId -> EnvValidity
validWhere valid_where = mempty { valid_where }

