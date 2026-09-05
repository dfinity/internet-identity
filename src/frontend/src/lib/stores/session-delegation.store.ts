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
import { currentDeviceId } from "$lib/stores/browser-key.store";
import { purgeAppSessions } from "$lib/stores/app-session.store";
import { canisterId, agentOptions } from "$lib/globals";
import {
  mintSessionDelegation,
  sessionDelegationIdentity,
  type SessionDelegationRecord,
} from "$lib/utils/authentication/sessionDelegation";

const SESSION_DELEGATION_STORE = createStore("ii-session-delegations", "keys");

// Treat the last 5 minutes of a delegation's lifetime as already expired:
// avoids serving a record that's "valid" at the FE check but expires
// between dispatch and IC validation (network latency + ingress queue
// + browser clock skew). Cleaner UX to fast-fail to a ceremony than to
// surface an InvalidDelegation error mid-call.
const EXPIRY_MARGIN_MS = 5 * 60 * 1000;

export const mintSession = async ({
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

export const purgeSession = async (identityNumber: bigint): Promise<void> => {
  await idbDel(identityNumber.toString(), SESSION_DELEGATION_STORE);
};

// Resolves an actor authorized as the given identity. Prefers the live
// authenticated actor (covers the immediate-post-ceremony race where
// mintSession is fire-and-forget and the IDB write may not have landed yet)
// and falls back to a stored session delegation; returns undefined if
// neither source has an actor for this identity.
export const actorForIdentity = async (
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
    void purgeSession(identityNumber);
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
    void agent.fetchSubnetKeys({ canisterId }).catch(() => {});
    return Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId,
    });
  } catch {
    void purgeSession(identityNumber);
    return undefined;
  }
};

/**
 * Forgets an identity on this device, and signs it out of every app it is signed into
 * from here.
 *
 * Dropping the local records alone would only stop II from silently signing the user
 * back in: the apps hold delegation chains rooted at session records the canister still
 * has, and go on refreshing against them until they expire. Ending this browser's
 * sessions for this identity is what makes "forget" mean signed out.
 *
 * Sessions are per browser and per identity, so this leaves other identities on this
 * browser, and this identity on the user's other browsers, alone.
 */
export const forgetIdentity = async (identityNumber: bigint): Promise<void> => {
  const deviceId = await currentDeviceId(identityNumber);
  const actor =
    deviceId === undefined ? undefined : await actorForIdentity(identityNumber);
  if (deviceId !== undefined && actor !== undefined) {
    try {
      await actor.revoke_device_sessions({
        identity_number: identityNumber,
        device_id: deviceId,
      });
    } catch {
      // The local records go either way. Keeping them because the canister could not be
      // reached would leave II able to sign the user back in silently, which is the
      // thing the user asked it to stop doing.
    }
  }
  await purgeSession(identityNumber);
  await purgeAppSessions(identityNumber);
};
