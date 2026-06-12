import {
  createStore,
  get as idbGet,
  set as idbSet,
  del as idbDel,
} from "idb-keyval";
import { get } from "svelte/store";
import { Actor, ActorSubclass, HttpAgent } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import { authenticationStore } from "$lib/stores/authentication.store";
import { canisterId, agentOptions } from "$lib/globals";
import {
  mintSessionDelegation,
  sessionDelegationIdentity,
  type SessionDelegationRecord,
} from "$lib/utils/authentication/sessionDelegation";

const SESSION_DELEGATION_STORE = createStore("ii-session-delegations", "keys");

const EXPIRY_MARGIN_MS = 5 * 60 * 1000;

export const mintAndStore = async ({
  identityNumber,
  actor,
}: {
  identityNumber: bigint;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => {
  try {
    const record = await mintSessionDelegation({ identityNumber, actor });
    await idbSet(identityNumber.toString(), record, SESSION_DELEGATION_STORE);
  } catch {
    // Degrade gracefully: ceremony-based auth still works without a session delegation.
  }
};

export const purge = async (identityNumber: bigint): Promise<void> => {
  await idbDel(identityNumber.toString(), SESSION_DELEGATION_STORE);
};

export const actorForAccountManagement = async (
  identityNumber: bigint,
): Promise<ActorSubclass<_SERVICE> | undefined> => {
  const authenticated = get(authenticationStore);
  if (
    authenticated !== undefined &&
    authenticated.identityNumber === identityNumber
  ) {
    return authenticated.actor;
  }

  let record: SessionDelegationRecord | undefined;
  try {
    record = await idbGet<SessionDelegationRecord>(
      identityNumber.toString(),
      SESSION_DELEGATION_STORE,
    );
  } catch {
    return undefined;
  }

  if (record === undefined) {
    return undefined;
  }

  if (record.expiresAtMillis - EXPIRY_MARGIN_MS <= Date.now()) {
    void purge(identityNumber);
    return undefined;
  }

  try {
    const identity = await sessionDelegationIdentity(
      record.keyPair,
      record.chainJson,
    );
    const agent = HttpAgent.createSync({ ...agentOptions, identity });
    // Best-effort prefetch to speed up query verification; the query path
    // fetches lazily if this fails, so a rejection here is non-fatal.
    void agent.fetchSubnetKeys(canisterId).catch(() => {});
    return Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId,
    });
  } catch {
    void purge(identityNumber);
    return undefined;
  }
};
