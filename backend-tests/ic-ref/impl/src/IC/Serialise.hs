{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{- |
This module defines Serialise instances of the IC state.

We put them into their own module, despite the usual advise against orphan
instances, to emphasize that these are not part of the `IC.Ref` module with its
“reference implementation” status.

Also, orphan instances are kinda ok in applications.

-}

module IC.Serialise () where

import Codec.Serialise
import GHC.Generics

import qualified IC.Wasm.Winter as W
import Control.Monad.Random.Lazy
import System.Random.Internal (StdGen(..))
import System.Random.SplitMix

import IC.Types
import IC.Wasm.Winter.Persist
import IC.Purify
import IC.Canister.Snapshot
import IC.Canister
import IC.Ref
import IC.Crypto
import qualified IC.Crypto.BLS as BLS

instance Serialise W.Value

deriving instance Generic Timestamp
instance Serialise Timestamp where

deriving instance Generic Responded
instance Serialise Responded where

deriving instance Generic RejectCode
instance Serialise RejectCode where

deriving instance Generic Response
instance Serialise Response where

deriving instance Generic WasmClosure
instance Serialise WasmClosure where

deriving instance Generic Callback
instance Serialise Callback where

deriving instance Generic MethodCall
instance Serialise MethodCall where

deriving instance Generic PInstance
instance Serialise PInstance where

deriving instance Generic PModuleInst
instance Serialise PModuleInst where

deriving instance Generic (Snapshot a)
instance Serialise a => Serialise (Snapshot a) where

deriving instance Generic IC
instance Serialise IC where

deriving instance Generic CallContext
instance Serialise CallContext where

deriving instance Generic Message
instance Serialise Message where

deriving instance Generic RequestStatus
instance Serialise RequestStatus where

deriving instance Generic CallResponse
instance Serialise CallResponse where

deriving instance Generic CallRequest
instance Serialise CallRequest where

deriving instance Generic RunStatus
instance Serialise RunStatus where

deriving instance Generic CanState
instance Serialise CanState where

deriving instance Generic CallOrigin
instance Serialise CallOrigin where

deriving instance Generic EntryPoint
instance Serialise EntryPoint where

instance Serialise CanisterContent where
    encode cc = encode
        ( raw_wasm (can_mod cc)
        , wsInstances (wasm_state cc)
        , wsStableMem (wasm_state cc)
        )
    decode = do
        (wasm, insts, sm) <- decode
        can_mod <- either fail pure $ parseCanister wasm
        -- There is some duplication here
        wasm_mod <- either fail pure $ W.parseModule wasm
        return $ CanisterContent
            { can_mod = can_mod
            , wasm_state = CanisterSnapshot
                { wsModule = wasm_mod
                , wsInstances = insts
                , wsStableMem = sm
                }
            }

deriving instance Serialise EntityId

deriving instance Serialise StdGen

instance Serialise SMGen where
    encode = encode  . unseedSMGen
    decode = seedSMGen' <$> decode

instance Serialise BLS.SecretKey
instance Serialise SecretKey where
    encode (BLS sk) = encode sk
    encode _ = error "IC.Serialise SecretKey: Only BLS supported"
    decode = BLS <$> decode
