{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Canister.Snapshot ( CanisterSnapshot(..) ) where

import Data.ByteString.Lazy (ByteString)

import IC.Types
import IC.Wasm.Winter (Module)
import IC.Wasm.Winter.Persist
import IC.Canister.Imp
import IC.Purify

data CanisterSnapshot = CanisterSnapshot
    { wsModule :: Module
    , wsInstances :: PInstance
    , wsStableMem :: ByteString
    } deriving Show

instance SnapshotAble ImpState where
    type SnapshotOf ImpState = CanisterSnapshot
    persist (ImpState _ inst sm mod) = do
        CanisterSnapshot mod <$> persistInstance inst <*> persistMemory sm
    recreate (CanisterSnapshot wasm_mod pinst pmem) = do
        rs <- rawInstantiate wasm_mod >>= trapToFail
        resumeInstance (isInstance rs) pinst
        resumeMemory (isStableMem rs) pmem
        return rs
      where
        trapToFail (Trap err) = fail $ "replay failed: " ++ show err
        trapToFail (Return x) = return x

deriving
  via (Snapshot CanisterSnapshot)
  instance Purify ImpState CanisterSnapshot
