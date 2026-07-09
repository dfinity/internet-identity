import { Principal } from "@icp-sdk/core/principal";
import { Actor } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { MAX_POLL_ATTEMPTS, pollDelay } from "$lib/utils/openidPoll";
import { DelegationChain, DelegationIdentity } from "@icp-sdk/core/identity";
import { Session } from "$lib/stores/session.store";

/**
 * Thrown by {@link authenticateWithSso} when the gated SSO login can't resolve
 * an existing identity because the org's stable identifier isn't `sub` and no
 * prior *normal* (primary-client) SSO login has bridged this user yet (§6.5).
 * The caller should prompt the user to sign in normally first.
 */
export class SsoNormalLoginRequiredError extends Error {
  constructor() {
    super("Sign in normally first before using this gated app");
    this.name = "SsoNormalLoginRequiredError";
  }
}

/**
 * SSO sign-in for a dapp origin — the IdP-side per-app gating path. Runs the
 * `sso_prepare_delegation` / `sso_get_delegation` pair, which enforces the gate
 * (`aud == the origin's declared client`) and binds the delegation to
 * `(sso_domain, origin)`. `openid_prepare_delegation` is untouched.
 *
 * `NoSuchAnchor` handling depends on whether the origin is `gated`:
 * - **gated** (per-app client): the user isn't bridged to a primary identity
 *   yet (non-`sub` org, §6.5) — throw {@link SsoNormalLoginRequiredError} so
 *   the UI prompts a normal sign-in first. A gated per-app client can't be used
 *   to register.
 * - **ungated** (primary client): a brand-new user — surface the raw
 *   `NoSuchAnchor` canister error so the caller routes to registration, exactly
 *   like the openid path.
 *
 * @throws {SsoNormalLoginRequiredError} for a gated login with no bridged
 *   identity yet.
 */
export const authenticateWithSso = async ({
  canisterId,
  session,
  jwt,
  discoveryDomain,
  origin,
  gated,
}: {
  canisterId: Principal;
  session: Session;
  jwt: string;
  discoveryDomain: string;
  origin: string;
  gated: boolean;
}): Promise<{
  identity: DelegationIdentity;
  identityNumber: bigint;
}> => {
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: session.agent,
    canisterId,
  });
  const sessionKey = new Uint8Array(session.identity.getPublicKey().toDer());

  for (let attempt = 0; attempt < MAX_POLL_ATTEMPTS; attempt++) {
    const prepared = await actor.sso_prepare_delegation(
      jwt,
      session.salt,
      sessionKey,
      discoveryDomain,
      origin,
    );
    if ("Pending" in prepared) {
      await pollDelay();
      continue;
    }
    if (gated && "Err" in prepared && "NoSuchAnchor" in prepared.Err) {
      throw new SsoNormalLoginRequiredError();
    }
    const {
      anchor_number: identityNumber,
      expiration,
      user_key,
    } = await throwCanisterError(prepared);

    const delegation = await actor.sso_get_delegation(
      jwt,
      session.salt,
      sessionKey,
      expiration,
      discoveryDomain,
      origin,
    );
    if ("Pending" in delegation) {
      await pollDelay();
      continue;
    }
    const signedDelegation = await throwCanisterError(delegation);
    const transformedDelegation = transformSignedDelegation(signedDelegation);
    const delegationChain = DelegationChain.fromDelegations(
      [transformedDelegation],
      new Uint8Array(user_key),
    );
    const identity = DelegationIdentity.fromDelegation(
      session.identity,
      delegationChain,
    );
    return { identity, identityNumber };
  }

  throw new Error("Timed out waiting for SSO discovery to complete");
};

export const authenticateWithJWT = async ({
  canisterId,
  session,
  jwt,
  discoveryDomain,
}: {
  canisterId: Principal;
  session: Session;
  jwt: string;
  discoveryDomain?: string;
}): Promise<{
  identity: DelegationIdentity;
  identityNumber: bigint;
}> => {
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: session.agent,
    canisterId,
  });
  const sessionKey = new Uint8Array(session.identity.getPublicKey().toDer());
  const domain: [] | [string] =
    discoveryDomain !== undefined ? [discoveryDomain] : [];

  for (let attempt = 0; attempt < MAX_POLL_ATTEMPTS; attempt++) {
    // `prepare` (update) drives the SSO discovery/JWKS fetch; `get` (query)
    // reads the cached result. Either reports `Pending` while the cache is
    // cold — re-call `prepare` to keep the fetch moving, then retry.
    const prepared = await actor.openid_prepare_delegation(
      jwt,
      session.salt,
      sessionKey,
      domain,
    );
    if ("Pending" in prepared) {
      await pollDelay();
      continue;
    }
    const {
      anchor_number: identityNumber,
      expiration,
      user_key,
    } = await throwCanisterError(prepared);

    const delegation = await actor.openid_get_delegation(
      jwt,
      session.salt,
      sessionKey,
      expiration,
      domain,
    );
    if ("Pending" in delegation) {
      await pollDelay();
      continue;
    }
    const signedDelegation = await throwCanisterError(delegation);
    const transformedDelegation = transformSignedDelegation(signedDelegation);
    const delegationChain = DelegationChain.fromDelegations(
      [transformedDelegation],
      new Uint8Array(user_key),
    );
    const identity = DelegationIdentity.fromDelegation(
      session.identity,
      delegationChain,
    );
    return { identity, identityNumber };
  }

  throw new Error("Timed out waiting for SSO discovery to complete");
};
