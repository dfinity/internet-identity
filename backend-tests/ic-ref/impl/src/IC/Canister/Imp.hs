{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
The canister interface, presented imperatively (or impurely), i.e. without rollback
-}
module IC.Canister.Imp
 ( CanisterEntryPoint
 , ImpState(..)
 , rawInstantiate
 , rawInitialize
 , rawQuery
 , rawUpdate
 , rawCallback
 , rawCleanup
 , rawPreUpgrade
 , rawPostUpgrade
 , rawInspectMessage
 )
where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy.UTF8 as BSU
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Except
import Data.STRef
import Data.Maybe
import Data.Int -- TODO: Should be Word32 in most cases
import Data.Word
import Data.Functor
import Numeric.Natural

import IC.Types
import IC.Constants
import IC.Wasm.Winter
import IC.Wasm.WinterMemory as Mem
import IC.Wasm.Imports

-- Parameters are the data that come from the caller

data Params = Params
  { param_dat  :: Maybe Blob
  , param_caller :: Maybe EntityId
  , reject_code :: Maybe Natural
  , reject_message :: Maybe String
  , cycles_refunded :: Maybe Cycles
  }

-- The execution state is all information available to the
-- canister. Some of it is immutable (could be separated here)

data ExecutionState s = ExecutionState
  { inst :: Instance s
  , stableMem :: Memory s
  , params :: Params
  , method_name :: Maybe MethodName
  , env :: Env
  -- now the mutable parts
  , cycles_available :: Maybe Cycles
  , cycles_accepted :: Cycles
  , balance :: Cycles
  , responded :: Responded
  , response :: Maybe Response
  , reply_data :: Blob
  , pending_call :: Maybe MethodCall
  , calls :: [MethodCall]
  , new_certified_data :: Maybe Blob
  , accepted :: Bool -- for canister_inspect_message
  }


initialExecutionState :: Instance s -> Memory s -> Env -> Responded -> ExecutionState s
initialExecutionState inst stableMem env responded = ExecutionState
  { inst
  , stableMem
  , params = Params Nothing Nothing Nothing Nothing Nothing
  , method_name = Nothing
  , env
  , cycles_available = Nothing
  , balance = env_balance env
  , cycles_accepted = 0
  , responded
  , response = Nothing
  , reply_data = mempty
  , pending_call = Nothing
  , calls = mempty
  , new_certified_data = Nothing
  , accepted = False
  }

-- Some bookkeeping to access the ExecutionState
--
-- We “always” have the 'STRef', but only within 'withES' is it actually
-- present.

type ESRef s = STRef s (Maybe (ExecutionState s))

newESRef :: ST s (ESRef s)
newESRef = newSTRef Nothing

-- | runs a computation with the given initial execution state
-- and returns the final execution state with it.
withES :: PrimMonad m =>
  ESRef (PrimState m) ->
  ExecutionState (PrimState m) ->
  m a -> m (a, ExecutionState (PrimState m))
withES esref es f = do
  before <- stToPrim $ readSTRef esref
  unless (isNothing before) $ error "withES with non-empty es"
  stToPrim $ writeSTRef esref $ Just es
  x <- f
  es' <- stToPrim $ readSTRef esref
  case es' of
    Nothing -> error "withES: ExecutionState lost"
    Just es' -> do
      stToPrim $ writeSTRef esref Nothing
      return (x, es')

getsES :: ESRef s -> (ExecutionState s -> b) -> HostM s b
getsES esref f = lift (readSTRef esref) >>= \case
  Nothing -> throwError "System API not available yet"
  Just es -> return (f es)

modES :: ESRef s -> (ExecutionState s -> ExecutionState s) -> HostM s ()
modES esref f = lift $ modifySTRef esref (fmap f)

appendReplyData :: ESRef s -> Blob -> HostM s ()
appendReplyData esref dat = modES esref $ \es ->
  es { reply_data = reply_data es <> dat }

setResponse :: ESRef s -> Response -> HostM s ()
setResponse esref r = modES esref $ \es ->
  es { response = Just r }

appendCall :: ESRef s -> MethodCall -> HostM s ()
appendCall esref c = modES esref $ \es ->
  es { calls = calls es ++ [c] }

getAvailable :: ESRef s -> HostM s Cycles
getAvailable esref =
  getsES esref cycles_available >>=
    maybe (throwError "no cycles available") return

getRefunded :: ESRef s -> HostM s Cycles
getRefunded esref =
  getsES esref (cycles_refunded . params)  >>=
    maybe (throwError "no cycles refunded") return

addBalance :: ESRef s -> Cycles -> HostM s ()
addBalance esref f = modES esref $ \es ->
  es { balance = min (balance es + f) cMAX_CANISTER_BALANCE  }

addAccepted :: ESRef s -> Cycles -> HostM s ()
addAccepted esref f = modES esref $ \es ->
  es { cycles_accepted = cycles_accepted es + f }

subtractBalance :: ESRef s -> Cycles -> HostM s ()
subtractBalance esref f = do
  current_balance <- getsES esref balance
  if f <= current_balance
  then modES esref $ \es -> es { balance = current_balance - f }
  else throwError "insufficient cycles to put on call"

subtractAvailable :: ESRef s -> Cycles -> HostM s ()
subtractAvailable esref f = do
  current <- getAvailable esref
  when (f > current) $ error "internal error: insufficient cycles to accept"
  modES esref $ \es -> es { cycles_available = Just (current - f) }

-- The System API, with all imports

-- The code is defined in the where clause to scope over the 'ESRef'

systemAPI :: forall s. ESRef s -> Imports s
systemAPI esref =
  [ toImport "ic0" "msg_arg_data_size" msg_arg_data_size
  , toImport "ic0" "msg_arg_data_copy" msg_arg_data_copy
  , toImport "ic0" "msg_caller_size" msg_caller_size
  , toImport "ic0" "msg_caller_copy" msg_caller_copy
  , toImport "ic0" "msg_reject_code" msg_reject_code
  , toImport "ic0" "msg_reject_msg_size" msg_reject_msg_size
  , toImport "ic0" "msg_reject_msg_copy" msg_reject_msg_copy

  , toImport "ic0" "msg_reply_data_append" msg_reply_data_append
  , toImport "ic0" "msg_reply" msg_reply
  , toImport "ic0" "msg_reject" msg_reject

  , toImport "ic0" "canister_self_copy" canister_self_copy
  , toImport "ic0" "canister_self_size" canister_self_size
  , toImport "ic0" "canister_status" canister_status

  , toImport "ic0" "msg_cycles_available" msg_cycles_available
  , toImport "ic0" "msg_cycles_refunded" msg_cycles_refunded
  , toImport "ic0" "msg_cycles_accept" msg_cycles_accept
  , toImport "ic0" "canister_cycle_balance" canister_cycle_balance

  , toImport "ic0" "call_new" call_new
  , toImport "ic0" "call_on_cleanup" call_on_cleanup
  , toImport "ic0" "call_data_append" call_data_append
  , toImport "ic0" "call_cycles_add" call_cycles_add
  , toImport "ic0" "call_perform" call_perform

  , toImport "ic0" "stable_size" stable_size
  , toImport "ic0" "stable_grow" stable_grow
  , toImport "ic0" "stable_write" stable_write
  , toImport "ic0" "stable_read" stable_read

  , toImport "ic0" "certified_data_set" certified_data_set
  , toImport "ic0" "data_certificate_present" data_certificate_present
  , toImport "ic0" "data_certificate_size" data_certificate_size
  , toImport "ic0" "data_certificate_copy" data_certificate_copy

  , toImport "ic0" "msg_method_name_size" msg_method_name_size
  , toImport "ic0" "msg_method_name_copy" msg_method_name_copy
  , toImport "ic0" "accept_message" accept_message

  , toImport "ic0" "time" get_time

  , toImport "ic0" "debug_print" debug_print
  , toImport "ic0" "trap" explicit_trap
  ]
  where
    -- Utilities
    gets :: (ExecutionState s -> b) -> HostM s b
    gets = getsES esref

    copy_to_canister :: Int32 -> Int32 -> Int32 -> Blob -> HostM s ()
    copy_to_canister dst offset size blob = do
      unless (offset == 0) $
        throwError "offset /= 0 not supported"
      unless (size == fromIntegral (BS.length blob)) $
        throwError "copying less than the full blob is not supported"
      i <- getsES esref inst
      -- TODO Bounds checking
      setBytes i (fromIntegral dst) blob

    copy_from_canister :: String -> Int32 -> Int32 -> HostM s Blob
    copy_from_canister _name src size = do
      i <- gets inst
      getBytes i (fromIntegral src) size

    size_and_copy :: HostM s Blob ->
      ( () -> HostM s Int32
      , (Int32, Int32, Int32) -> HostM s ()
      )
    size_and_copy get_blob =
      ( \() ->
        get_blob >>= \blob -> return $ fromIntegral (BS.length blob)
      , \(dst, offset, size) ->
        get_blob >>= \blob -> copy_to_canister dst offset size blob
      )

    -- Unsafely print
    putBytes :: BS.ByteString -> HostM s ()
    putBytes bytes =
      unsafeIOToPrim $ BSC.putStrLn $ BSC.pack "debug.print: " <> bytes

    -- The system calls (in the order of the public spec)
    -- https://docs.dfinity.systems/spec/public/#_system_imports

    msg_arg_data_size :: () -> HostM s Int32
    msg_arg_data_copy :: (Int32, Int32, Int32) -> HostM s ()
    (msg_arg_data_size, msg_arg_data_copy) = size_and_copy $
        gets (param_dat . params) >>= maybe (throwError "No argument") return

    msg_caller_size :: () -> HostM s Int32
    msg_caller_copy :: (Int32, Int32, Int32) -> HostM s ()
    (msg_caller_size, msg_caller_copy) = size_and_copy $
      gets (param_caller . params)
        >>= maybe (throwError "No argument") (return . rawEntityId)

    msg_reject_code :: () -> HostM s Int32
    msg_reject_code () =
      gets (reject_code . params)
        >>= maybe (throwError "No reject code") (return . fromIntegral)

    msg_reject_msg_size :: () -> HostM s Int32
    msg_reject_msg_copy :: (Int32, Int32, Int32) -> HostM s ()
    (msg_reject_msg_size, msg_reject_msg_copy) = size_and_copy $ do
      gets (reject_message . params)
        >>= maybe (throwError "No reject code") (return . BSU.fromString)

    assert_not_responded :: HostM s ()
    assert_not_responded = do
      gets responded >>= \case
        Responded False -> return ()
        Responded True  -> throwError "This call has already been responded to earlier"
      gets response >>= \case
        Nothing -> return ()
        Just  _ -> throwError "This call has already been responded to in this function"

    msg_reply_data_append :: (Int32, Int32) -> HostM s ()
    msg_reply_data_append (src, size) = do
      assert_not_responded
      bytes <- copy_from_canister "msg_reply_data_append" src size
      appendReplyData esref bytes

    msg_reply :: () -> HostM s ()
    msg_reply () = do
      assert_not_responded
      bytes <- gets reply_data
      setResponse esref (Reply bytes)

    msg_reject :: (Int32, Int32) -> HostM s ()
    msg_reject (src, size) = do
      assert_not_responded
      bytes <- copy_from_canister "msg_reject" src size
      let msg = BSU.toString bytes
      setResponse esref $ Reject (RC_CANISTER_REJECT, msg)

    canister_self_size :: () -> HostM s Int32
    canister_self_copy :: (Int32, Int32, Int32) -> HostM s ()
    (canister_self_size, canister_self_copy) = size_and_copy $
      rawEntityId <$> gets (env_self . env)

    canister_status :: () -> HostM s Int32
    canister_status () = gets (env_status . env) <&> \case
        Running -> 1
        Stopping -> 2
        Stopped -> 3

    msg_cycles_refunded :: () -> HostM s Word64
    msg_cycles_refunded () = fromIntegral <$> getRefunded esref

    msg_cycles_available :: () -> HostM s Word64
    msg_cycles_available () = fromIntegral <$> getAvailable esref

    msg_cycles_accept :: Word64 -> HostM s Word64
    msg_cycles_accept max_amount = do
      available <- fromIntegral <$> getAvailable esref
      balance <- gets balance
      let amount = minimum
            [ fromIntegral max_amount
            , available
            , cMAX_CANISTER_BALANCE - balance]
      subtractAvailable esref amount
      addBalance esref amount
      addAccepted esref amount
      return (fromIntegral amount)

    canister_cycle_balance :: () -> HostM s Word64
    canister_cycle_balance () = fromIntegral <$> gets balance

    call_new :: ( Int32, Int32, Int32, Int32, Int32, Int32, Int32, Int32 ) -> HostM s ()
    call_new ( callee_src, callee_size, name_src, name_size
             , reply_fun, reply_env, reject_fun, reject_env ) = do
      discard_pending_call
      callee <- copy_from_canister "call_simple" callee_src callee_size
      method_name <- copy_from_canister "call_simple" name_src name_size
      let reply_closure = WasmClosure reply_fun reply_env
      let reject_closure = WasmClosure reject_fun reject_env
      setPendingCall $ MethodCall
        { call_callee = EntityId callee
        , call_method_name = BSU.toString method_name -- TODO: check for valid UTF8
        , call_arg = mempty
        , call_callback = Callback reply_closure reject_closure Nothing
        , call_transferred_cycles = 0
        }

    call_on_cleanup :: (Int32, Int32) -> HostM s ()
    call_on_cleanup (fun, env) = do
      let cleanup_closure = WasmClosure fun env
      changePendingCall $ \pc -> do
        let callback = call_callback pc
        when (isJust (cleanup_callback callback)) $
            throwError "call_on_cleanup invoked twice"
        return $ pc { call_callback = callback { cleanup_callback = Just cleanup_closure } }

    call_data_append :: (Int32, Int32) -> HostM s ()
    call_data_append (src, size) = do
      arg <- copy_from_canister "call_data_append" src size
      changePendingCall $ \pc -> return $ pc { call_arg = call_arg pc <> arg }

    call_cycles_add :: Word64 -> HostM s ()
    call_cycles_add amount = do
      let cycles = fromIntegral amount
      changePendingCall $ \pc -> do
        subtractBalance esref cycles
        return $ pc { call_transferred_cycles = call_transferred_cycles pc + cycles }

    call_perform :: () -> HostM s Int32
    call_perform () = do
      pc <- getPendingCall

      appendCall esref pc
      modES esref $ \es -> es { pending_call = Nothing }
      return 0

    -- utilities for the pending call

    setPendingCall :: MethodCall -> HostM s ()
    setPendingCall pc =
      modES esref $ \es -> es { pending_call = Just pc }

    getPendingCall :: HostM s MethodCall
    getPendingCall =
      gets pending_call >>= \case
        Nothing -> throwError "No call in process"
        Just pc -> return pc

    changePendingCall :: (MethodCall -> HostM s MethodCall) -> HostM s ()
    changePendingCall act =
      getPendingCall >>= act >>= setPendingCall

    discard_pending_call = do
      mpc <- gets pending_call
      forM_ mpc $ \pc -> addBalance esref (call_transferred_cycles pc)
      modES esref $ \es -> es { pending_call = Nothing }

    stable_size :: () -> HostM s Int32
    stable_size () = do
      m <- gets stableMem
      Mem.size m

    stable_grow :: Int32 -> HostM s Int32
    stable_grow delta = do
      m <- gets stableMem
      Mem.grow m delta

    stable_write :: (Int32, Int32, Int32) -> HostM s ()
    stable_write (dst, src, size) = do
      m <- gets stableMem
      i <- getsES esref inst
      blob <- getBytes i (fromIntegral src) size
      Mem.write m (fromIntegral dst) blob

    stable_read :: (Int32, Int32, Int32) -> HostM s ()
    stable_read (dst, src, size) = do
      m <- gets stableMem
      i <- getsES esref inst
      blob <- Mem.read m (fromIntegral src) size
      setBytes i (fromIntegral dst) blob

    certified_data_set :: (Int32, Int32) -> HostM s ()
    certified_data_set (src, size) = do
      when (size > 32) $ throwError "certified_data_set: too large"
      blob <- copy_from_canister "certified_data_set" src size
      modES esref $ \es -> es { new_certified_data = Just blob }

    data_certificate_present :: () -> HostM s Int32
    data_certificate_present () =
      gets (env_certificate . env) >>= \case
        Just _ -> return 1
        Nothing -> return 0

    data_certificate_size :: () -> HostM s Int32
    data_certificate_copy :: (Int32, Int32, Int32) -> HostM s ()
    (data_certificate_size, data_certificate_copy) = size_and_copy $
      gets (env_certificate . env) >>= \case
        Just b -> return b
        Nothing -> throwError "no certificate available"

    msg_method_name_size :: () -> HostM s Int32
    msg_method_name_copy :: (Int32, Int32, Int32) -> HostM s ()
    (msg_method_name_size, msg_method_name_copy) = size_and_copy $
      gets method_name >>=
        maybe (throwError "Cannot query method name here")
              (return . BS.fromStrict . T.encodeUtf8 . T.pack)

    accept_message :: () -> HostM s ()
    accept_message () = do
      a <- gets accepted
      when a $ throwError "Message already accepted"
      modES esref $ \es -> es { accepted = True }

    get_time :: () -> HostM s Word64
    get_time () = do
        Timestamp ns <- gets (env_time . env)
        return (fromIntegral ns)

    debug_print :: (Int32, Int32) -> HostM s ()
    debug_print (src, size) = do
      -- TODO: This should be a non-trapping copy
      bytes <- copy_from_canister "debug_print" src size
      putBytes bytes

    explicit_trap :: (Int32, Int32) -> HostM s ()
    explicit_trap (src, size) = do
      -- TODO: This should be a non-trapping copy
      bytes <- copy_from_canister "trap" src size
      let msg = BSU.toString bytes
      throwError $ "canister trapped explicitly: " ++ msg

-- The state of an instance, consistig of
--  * the underlying Wasm state,
--  * additional remembered information like the CanisterId
--  * the 'ESRef' that the system api functions are accessing
--  * the original module (so that this ImpState can be snapshotted)

data ImpState s = ImpState
  { isESRef :: ESRef s
  , isInstance :: Instance s
  , isStableMem :: Memory s
  , isModule :: Module
  }

rawInstantiate :: Module -> ST s (TrapOr (ImpState s))
rawInstantiate wasm_mod = do
  esref <- newESRef
  result <- runExceptT $ (,)
    <$> initialize wasm_mod (systemAPI esref)
    <*> Mem.new
  case result of
    Left  err -> return $ Trap err
    Right (inst, sm) -> return $ Return $ ImpState esref inst sm wasm_mod

cantRespond :: Responded
cantRespond = Responded True

canRespond :: Responded
canRespond = Responded False

canisterActions :: ExecutionState s -> CanisterActions
canisterActions es = CanisterActions
    { set_certified_data = new_certified_data es
    }

type CanisterEntryPoint r = forall s. (ImpState s -> ST s r)

rawInitialize :: EntityId -> Env -> Blob -> ImpState s -> ST s (TrapOr CanisterActions)
rawInitialize caller env dat (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , cycles_refunded = Nothing
                  }
              }

    --  invoke canister_init
    if "canister_init" `elem` exportedFunctions wasm_mod
    then withES esref es $ void $ invokeExport inst "canister_init" []
    else return ((), es)

  case result of
    Left  err -> return $ Trap err
    Right (_, es')
        | accepted es' -> return $ Trap "cannot accept_message here"
        | not (null (calls es')) -> return $ Trap "cannot call from init"
        | otherwise        -> return $ Return $ canisterActions es'

rawPreUpgrade :: EntityId -> Env -> ImpState s -> ST s (TrapOr (CanisterActions, Blob))
rawPreUpgrade caller env (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond)
              { params = Params
                  { param_dat    = Nothing
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , cycles_refunded = Nothing
                  }
              }

    if "canister_pre_upgrade" `elem` exportedFunctions wasm_mod
    then withES esref es $ void $ invokeExport inst "canister_pre_upgrade" []
    else return ((), es)

  case result of
    Left  err -> return $ Trap err
    Right (_, es')
        | accepted es' -> return $ Trap "cannot accept_message here"
        | not (null (calls es')) -> return $ Trap "cannot call from pre_upgrade"
        | otherwise -> do
            stable_mem <- Mem.export (stableMem es')
            return $ Return (canisterActions es', stable_mem)

rawPostUpgrade :: EntityId -> Env -> Blob -> Blob -> ImpState s -> ST s (TrapOr CanisterActions)
rawPostUpgrade caller env mem dat (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , cycles_refunded = Nothing
                  }
              }
    lift $ Mem.imp (stableMem es) mem

    if "canister_post_upgrade" `elem` exportedFunctions wasm_mod
    then withES esref es $ void $ invokeExport inst "canister_post_upgrade" []
    else return ((), es)

  case result of
    Left  err -> return $ Trap err
    Right (_, es')
        | accepted es' -> return $ Trap "cannot accept_message here"
        | not (null (calls es')) -> return $ Trap "cannot call from post_upgrade"
        | otherwise -> return $ Return (canisterActions es')

rawQuery :: MethodName -> EntityId -> Env -> Blob -> ImpState s -> ST s (TrapOr Response)
rawQuery method caller env dat (ImpState esref inst sm _) = do
  let es = (initialExecutionState inst sm env canRespond)
            { params = Params
                { param_dat    = Just dat
                , param_caller = Just caller
                , reject_code  = Nothing
                , reject_message = Nothing
                , cycles_refunded = Nothing
                }
            }
  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_query " ++ method) []

  case result of
    Left err -> return $ Trap err
    Right (_, es')
      | Just _ <- new_certified_data es'
        -> return $ Trap "Cannot set certified data from a query method"
      | not (null (calls es')) -> return $ Trap "cannot call from query"
      | accepted es' -> return $ Trap "cannot accept_message here"
      | Just r <- response es' -> return $ Return r
      | otherwise -> return $ Trap "No response"

rawUpdate :: MethodName -> EntityId -> Env -> Responded -> Cycles -> Blob -> ImpState s -> ST s (TrapOr UpdateResult)
rawUpdate method caller env responded cycles_available dat (ImpState esref inst sm _) = do
  let es = (initialExecutionState inst sm env responded)
            { params = Params
                { param_dat    = Just dat
                , param_caller = Just caller
                , reject_code  = Nothing
                , reject_message = Nothing
                , cycles_refunded = Nothing
                }
            , cycles_available = Just cycles_available
            }

  result <- runExceptT $ withES esref es $
    invokeExport inst ("canister_update " ++ method) []
  case result of
    Left  err -> return $ Trap err
    Right (_, es')
      | accepted es' -> return $ Trap "cannot accept_message here"
      | otherwise    -> return $ Return
        ( CallActions (calls es') (cycles_accepted es') (response es')
        , canisterActions es'
        )

rawCallback :: Callback -> Env -> Responded -> Cycles -> Response -> Cycles -> ImpState s -> ST s (TrapOr UpdateResult)
rawCallback callback env responded cycles_available res refund (ImpState esref inst sm _) = do
  let params = case res of
        Reply dat ->
          Params { param_dat = Just dat, param_caller = Nothing, reject_code = Just 0, reject_message = Nothing, cycles_refunded = Just refund }
        Reject (rc, reject_message) ->
          Params { param_dat = Nothing, param_caller = Nothing, reject_code = Just (rejectCode rc), reject_message = Just reject_message, cycles_refunded = Just refund }
  let es = (initialExecutionState inst sm env responded)
            { params
            , cycles_available = Just cycles_available
            }

  let WasmClosure fun_idx env = case res of
        Reply {}  -> reply_callback callback
        Reject {} -> reject_callback callback

  result <- runExceptT $ withES esref es $
    invokeTable inst fun_idx [I32 env]
  case result of
    Left  err -> return $ Trap err
    Right (_, es')
      | accepted es' -> return $ Trap "cannot accept_message here"
      | otherwise    -> return $ Return
        ( CallActions (calls es') (cycles_accepted es') (response es')
        , canisterActions es'
        )

-- Needs to be separate from rawCallback, as it is its own transaction
rawCleanup :: WasmClosure -> Env -> ImpState s -> ST s (TrapOr ())
rawCleanup (WasmClosure fun_idx cb_env) env (ImpState esref inst sm _) = do
  let es = initialExecutionState inst sm env cantRespond

  result <- runExceptT $ withES esref es $
    invokeTable inst fun_idx [I32 cb_env]
  case result of
    Left  err -> return $ Trap err
    Right (_, es')
      | Just _ <- new_certified_data es'
        -> return $ Trap "Cannot set certified data from inspect_message"
      | not (null (calls es'))  -> return $ Trap "cannot call from inspect_message"
      | isJust (response es')   -> return $ Trap "cannot respond from inspect_message"
      | accepted es' -> return $ Trap "cannot accept_message here"
      | otherwise    -> return $ Return ()

rawInspectMessage :: MethodName -> EntityId -> Env -> Blob -> ImpState s -> ST s (TrapOr ())
rawInspectMessage method caller env dat (ImpState esref inst sm wasm_mod) = do
  result <- runExceptT $ do
    let es = (initialExecutionState inst sm env cantRespond)
              { params = Params
                  { param_dat    = Just dat
                  , param_caller = Just caller
                  , reject_code  = Nothing
                  , reject_message = Nothing
                  , cycles_refunded = Nothing
                  }
              , method_name = Just method
              }

    if "canister_inspect_message" `elem` exportedFunctions wasm_mod
    then withES esref es $ void $ invokeExport inst "canister_inspect_message" []
    else return ((), es { accepted = True } )

  case result of
    Left err -> return $ Trap err
    Right (_, es')
      | Just _ <- new_certified_data es'
        -> return $ Trap "Cannot set certified data from inspect_message"
      | not (null (calls es'))  -> return $ Trap "cannot call from inspect_message"
      | isJust (response es')   -> return $ Trap "cannot respond from inspect_message"
      | not (accepted es')      -> return $ Trap "message not accepted by inspect_message"
      | otherwise -> return $ Return ()
