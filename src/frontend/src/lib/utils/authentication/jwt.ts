import { Principal } from "@icp-sdk/core/principal";
import { Actor, type Identity } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import { MAX_POLL_ATTEMPTS, pollDelay } from "$lib/utils/openidPoll";
import {
  AttributesIdentity,
  DelegationChain,
  DelegationIdentity,
} from "@icp-sdk/core/identity";
import { Session } from "$lib/stores/session.store";

/**
 * Thrown while completing a gated SSO sign-up when the backend signals a normal
 * (primary-client) sign-in is required first — a non-`sub` org's first gated
 * login (see `AuthFlow` in `authFlow.svelte.ts`).
 */
export class SsoNormalLoginRequiredError extends Error {
  constructor() {
    super("Sign in normally first before using this gated app");
    this.name = "SsoNormalLoginRequiredError";
  }
}

/** SSO sign-in bound to a dapp origin; attaches the certified SSO attribute bundle to the returned identity. */
export const authenticateWithSso = async ({
  canisterId,
  session,
  jwt,
  discoveryDomain,
  origin,
}: {
  canisterId: Principal;
  session: Session;
  jwt: string;
  discoveryDomain: string;
  origin: string;
}): Promise<{
  identity: Identity;
  identityNumber: bigint;
}> => {
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: session.agent,
    canisterId,
  });
  const sessionKey = new Uint8Array(session.identity.getPublicKey().toDer());

  for (let attempt = 0; attempt < MAX_POLL_ATTEMPTS; attempt++) {
    const prepared = await actor.sso_prepare_delegation({
      jwt,
      salt: session.salt,
      session_key: sessionKey,
      org_domain: discoveryDomain,
      target_app_origin: origin,
    });
    if ("Pending" in prepared) {
      await pollDelay();
      continue;
    }
    const {
      anchor_number: identityNumber,
      expiration,
      user_key,
      sso_attr_bundle: ssoAttrBundle,
    } = await throwCanisterError(prepared);

    const delegation = await actor.sso_get_delegation({
      jwt,
      salt: session.salt,
      session_key: sessionKey,
      expiration,
      org_domain: discoveryDomain,
      target_app_origin: origin,
      sso_attr_bundle: ssoAttrBundle,
    });
    if ("Pending" in delegation) {
      await pollDelay();
      continue;
    }
    const { signed_delegation: signedDelegation, sso_attr_bundle_signature } =
      await throwCanisterError(delegation);
    const transformedDelegation = transformSignedDelegation(signedDelegation);
    const delegationChain = DelegationChain.fromDelegations(
      [transformedDelegation],
      new Uint8Array(user_key),
    );
    const delegationIdentity = DelegationIdentity.fromDelegation(
      session.identity,
      delegationChain,
    );
    // The bundle rides as `sender_info` on every call so attribute sites can certify `sso:<domain>` attributes.
    const identity = new AttributesIdentity({
      inner: delegationIdentity,
      attributes: {
        data: new Uint8Array(ssoAttrBundle),
        signature: new Uint8Array(sso_attr_bundle_signature),
      },
      signer: { canisterId },
    });
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
