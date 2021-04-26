{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
This module provides a way to persist the state of a Winter Wasm instance, and
to recover it.

It is tailored to the use by ic-ref. For example it assumes that the
table of a wasm instance is immutable.
-}
module IC.Wasm.Winter.Persist
  ( PInstance(..)
  , PModuleInst(..)
  , persistInstance
  , resumeInstance
  , persistMemory
  , resumeMemory
  )
  where

import Control.Monad
import Control.Monad.ST
import Data.Primitive.MutVar
import qualified Data.IntMap as IM
import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)

import qualified Wasm.Runtime.Global as W
import qualified Wasm.Runtime.Instance as W
import qualified Wasm.Runtime.Memory as W
import qualified Wasm.Syntax.Values as W
import qualified Wasm.Util.Source as W

import IC.Wasm.Winter (Instance)

-- |
-- This stores data read from an instance.
newtype PInstance = PInstance (Persisted (Instance ()))
  deriving Show

persistInstance :: Instance s -> ST s PInstance
persistInstance i = PInstance <$> persist i

resumeInstance :: Instance s -> PInstance -> ST s ()
resumeInstance i (PInstance p) = resume i p

persistMemory :: W.MemoryInst (ST s) -> ST s ByteString
persistMemory i = persist i

resumeMemory :: W.MemoryInst (ST s) -> ByteString -> ST s ()
resumeMemory i p = resume i p

class Monad (M a) => Persistable a where
  type Persisted a :: *
  type M a :: * -> *
  persist :: a -> M a (Persisted a)
  resume :: a -> Persisted a -> M a ()

instance Persistable (W.MemoryInst (ST s)) where
  type Persisted (W.MemoryInst (ST s)) = ByteString
  type M (W.MemoryInst (ST s)) = ST s
  persist = W.exportMemory
  resume = W.importMemory

instance Persistable (W.GlobalInst (ST s)) where
  type Persisted (W.GlobalInst (ST s)) = W.Value
  type M (W.GlobalInst (ST s)) = ST s
  persist m = readMutVar (W._giContent m)
  resume m = writeMutVar (W._giContent m)

data PModuleInst = PModuleInst
  { memories :: V.Vector (Persisted (W.MemoryInst (ST ())))
  , globals :: V.Vector (Persisted (W.GlobalInst (ST ())))
  }
  deriving Show

instance Persistable (W.ModuleInst W.Phrase (ST s)) where
  type Persisted (W.ModuleInst W.Phrase (ST s)) = PModuleInst
  type M (W.ModuleInst W.Phrase (ST s)) = ST s
  persist inst = PModuleInst
    <$> persist (W._miMemories inst)
    <*> persist (W._miGlobals inst)
  resume inst pinst = do
    resume (W._miMemories inst) (memories pinst)
    resume (W._miGlobals inst) (globals pinst)


instance Persistable a => Persistable [a] where
  type Persisted [a] = [Persisted a]
  type M [a] = M a
  persist = mapM persist
  resume xs ys = do
    unless (length xs == length ys) $ error "Lengths don’t match"
    zipWithM_ resume xs ys

instance Persistable a => Persistable (V.Vector a) where
  type Persisted (V.Vector a) = V.Vector (Persisted a)
  type M (V.Vector a) = M a
  persist = mapM persist
  resume xs ys = do
    unless (V.length xs == V.length ys) $ error "Lengths don’t match"
    V.zipWithM_ resume xs ys

instance (Eq k, Persistable a) => Persistable (M.Map k a) where
  type Persisted (M.Map k a) = M.Map k (Persisted a)
  type M (M.Map k a) = M a
  persist = mapM persist
  resume xs ys = do
    unless (M.keys xs == M.keys ys) $ error "Map keys don’t match"
    zipWithM_ resume (M.elems xs) (M.elems ys)

instance Persistable a => Persistable (IM.IntMap a) where
  type Persisted (IM.IntMap a) = M.Map Int (Persisted a)
  type M (IM.IntMap a) = M a
  persist = mapM persist . M.fromList . IM.toList
  resume xs ys = do
    let ys' = IM.fromList (M.toList ys)
    unless (IM.keys xs == IM.keys ys') $ error "Map keys don’t match"
    zipWithM_ resume (IM.elems xs) (IM.elems ys')

instance Persistable a => Persistable (a, Int) where
  type Persisted (a, Int) = Persisted a
  type M (a, Int) = M a
  persist (a, _i) = persist a
  resume (a, _i) p = resume a p
