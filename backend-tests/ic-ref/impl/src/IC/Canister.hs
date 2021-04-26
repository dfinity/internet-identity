{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module IC.Canister
    ( WasmState
    , parseCanister
    , CanisterModule(..)
    , InitFunc, UpdateFunc, QueryFunc
    , asUpdate
    )
    where

import qualified Data.Map as M
import Data.List

import IC.Types
import IC.Wasm.Winter (parseModule, exportedFunctions, Module)

import IC.Purify
import IC.Canister.Snapshot
import IC.Canister.Imp
import IC.Hash

-- Here we can swap out the purification machinery
type WasmState = CanisterSnapshot
-- type WasmState = Replay ImpState

type InitFunc = EntityId -> Env -> Blob -> TrapOr (WasmState, CanisterActions)
type UpdateFunc = WasmState -> TrapOr (WasmState, UpdateResult)
type QueryFunc = WasmState -> TrapOr Response

data CanisterModule = CanisterModule
  { raw_wasm :: Blob
  , raw_wasm_hash :: Blob -- just caching, it’s worth it
  , init_method :: InitFunc
  , update_methods :: MethodName ↦ (EntityId -> Env -> Responded -> Cycles -> Blob -> UpdateFunc)
  , query_methods :: MethodName ↦ (EntityId -> Env -> Blob -> QueryFunc)
  , callbacks :: Callback -> Env -> Responded -> Cycles -> Response -> Cycles -> UpdateFunc
  , cleanup :: WasmClosure -> Env -> WasmState -> TrapOr (WasmState, ())
  , pre_upgrade_method :: WasmState -> EntityId -> Env -> TrapOr (CanisterActions, Blob)
  , post_upgrade_method :: EntityId -> Env -> Blob -> Blob -> TrapOr (WasmState, CanisterActions)
  , inspect_message :: MethodName -> EntityId -> Env -> Blob -> WasmState -> TrapOr ()
  }

instance Show CanisterModule where
    show _ = "CanisterModule{...}"

parseCanister :: Blob -> Either String CanisterModule
parseCanister bytes =
  case parseModule bytes of
    Left  err -> Left err
    Right wasm_mod -> Right $ CanisterModule
      { raw_wasm = bytes
      , raw_wasm_hash = sha256 bytes
      , init_method = \caller env dat ->
            case instantiate wasm_mod of
              Trap err -> Trap err
              Return wasm_state0 ->
                invoke wasm_state0 (rawInitialize caller env dat)
      , update_methods = M.fromList
        [ (m,
          \caller env responded cycles_available dat wasm_state ->
          invoke wasm_state (rawUpdate m caller env responded cycles_available dat))
        | n <- exportedFunctions wasm_mod
        , Just m <- return $ stripPrefix "canister_update " n
        ]
      , query_methods = M.fromList
        [ (m, \caller env arg wasm_state ->
            snd <$> invoke wasm_state (rawQuery m caller env arg))
        | n <- exportedFunctions wasm_mod
        , Just m <- return $ stripPrefix "canister_query " n
        ]
      , callbacks = \cb env responded cycles_available res refund wasm_state ->
        invoke wasm_state (rawCallback cb env responded cycles_available res refund)
      , cleanup = \cb env wasm_state ->
        invoke wasm_state (rawCleanup cb env)
      , pre_upgrade_method = \wasm_state caller env ->
            snd <$> invoke wasm_state (rawPreUpgrade caller env)
      , post_upgrade_method = \caller env mem dat ->
            case instantiate wasm_mod of
              Trap err -> Trap err
              Return wasm_state0 ->
                invoke wasm_state0 (rawPostUpgrade caller env mem dat)
      , inspect_message = \method_name caller env arg wasm_state ->
            snd <$> invoke wasm_state (rawInspectMessage method_name caller env arg)
      }

instantiate :: Module -> TrapOr WasmState
instantiate wasm_mod =
  either Trap Return $ snd $ createMaybe $ do
    rawInstantiate wasm_mod >>= \case
      Trap err -> return ((), Left err)
      Return rs -> return ((), Right rs)

invoke :: WasmState -> CanisterEntryPoint (TrapOr r) -> TrapOr (WasmState, r)
invoke s f =
  case perform f s of
    (_, Trap msg) -> Trap msg
    (s', Return r) -> Return (s', r)

-- | Turns a query function into an update function
asUpdate ::
  (EntityId -> Env -> Blob -> QueryFunc) ->
  (EntityId -> Env -> Responded -> Cycles -> Blob -> UpdateFunc)
asUpdate f caller env (Responded responded) _cycles_available dat wasm_state
  | responded = error "asUpdate: responded == True"
  | otherwise =
    (\res -> (wasm_state, (noCallActions { ca_response = Just res }, noCanisterActions))) <$>
    f caller env dat wasm_state
