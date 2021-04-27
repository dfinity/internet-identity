{-
Plumbing related to Candid and the management canister.
-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module IC.Management where

import Codec.Candid
import IC.Types
import qualified Data.Row.Internal as R

-- This needs cleaning up
principalToEntityId :: Principal -> EntityId
principalToEntityId = EntityId . rawPrincipal

entityIdToPrincipal :: EntityId -> Principal
entityIdToPrincipal = Principal . rawEntityId

type InstallMode = [candidType|
    variant {install : null; reinstall : null; upgrade : null}
  |]

type RunState = [candidType|
    variant { running; stopping; stopped }
  |]

type Settings = [candidType|
    record {
      controller : opt principal;
      compute_allocation : opt nat;
      memory_allocation : opt nat;
      freezing_threshold : opt nat;
    }
  |]

type ICManagement m = [candidFile|ic.did|]

managementMethods :: [String]
managementMethods = R.labels @(ICManagement IO) @R.Unconstrained1
