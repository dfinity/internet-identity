import {
  createStore,
  get as idbGet,
  set as idbSet,
  del as idbDel,
} from "idb-keyval";
import { get } from "svelte/store";
import { z } from "zod";
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

// Treat the last 5 minutes of a delegation's lifetime as already expired:
// avoids serving a record that's "valid" at the FE check but expires
// between dispatch and IC validation (network latency + ingress queue
// + browser clock skew). Cleaner UX to fast-fail to a ceremony than to
// surface an InvalidDelegation error mid-call.
const EXPIRY_MARGIN_MS = 5 * 60 * 1000;

const SessionDelegationRecordSchema: z.ZodType<SessionDelegationRecord> =
  z.object({
    identityNumber: z.bigint(),
    keyPair: z.object({
      privateKey: z.instanceof(CryptoKey),
      publicKey: z.instanceof(CryptoKey),
    }),
    chainJson: z.string(),
    expiresAtMillis: z.number(),
  });

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

  let storedRecord: unknown;
  try {
    storedRecord = await idbGet(
      identityNumber.toString(),
      SESSION_DELEGATION_STORE,
    );
  } catch {
    return undefined;
  }

  if (storedRecord === undefined) {
    return undefined;
  }

  const parsedRecord = SessionDelegationRecordSchema.safeParse(storedRecord);
  if (!parsedRecord.success) {
    void purgeSession(identityNumber);
    return undefined;
  }
  const record = parsedRecord.data;

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
