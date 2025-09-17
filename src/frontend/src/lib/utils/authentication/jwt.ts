import { Principal } from "@dfinity/principal";
import { Actor, HttpAgent } from "@dfinity/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import {
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@dfinity/identity";
import { Session } from "$lib/stores/session.store";
import { restoreECDSAIdentity } from "../restoreECDSAIdentity";

export const authenticateWithJWT = async ({
  canisterId,
  session,
  jwt,
}: {
  canisterId: Principal;
  session: Session;
  jwt: string;
}): Promise<{
  identity: DelegationIdentity;
  identityNumber: bigint;
}> => {
  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent: session.agent,
    canisterId,
  });
  const sessionKey = new Uint8Array(session.identity.getPublicKey().toDer());
  const {
    anchor_number: identityNumber,
    expiration,
    user_key,
  } = await actor
    .openid_prepare_delegation(jwt, session.salt, sessionKey)
    .then(throwCanisterError);
  const signedDelegation = await actor
    .openid_get_delegation(jwt, session.salt, sessionKey, expiration)
    .then(throwCanisterError);
  const transformedDelegation = transformSignedDelegation(signedDelegation);
  const delegationChain = DelegationChain.fromDelegations(
    [transformedDelegation],
    new Uint8Array(user_key).buffer,
  );
  const identity = DelegationIdentity.fromDelegation(
    session.identity,
    delegationChain,
  );
  return { identity, identityNumber };
};

export const authenticateRedirectCallbackWithJWT = async ({
  canisterId,
  jwt,
  salt,
  identity,
}: {
  canisterId: Principal;
  jwt: string;
  salt: Uint8Array;
  identity: ECDSAKeyIdentity;
}): Promise<DelegationChain> => {
  console.log("📨 Starting redirect callback authentication");
  console.log("🎟️ JWT (first 50 chars):", jwt.slice(0, 50), "...");
  console.log("🧂 Salt (hex):", Buffer.from(salt).toString("hex"));
  console.log(
    "🔑 II Public Key (DER hex):",
    Buffer.from(identity.getPublicKey().toDer()).toString("hex"),
  );

  const publicKey = new Uint8Array(identity.getPublicKey().toDer());
  const agent = new HttpAgent({ identity });
  if (import.meta.env.DEV) await agent.fetchRootKey();

  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent,
    canisterId,
  });

  const { expiration, user_key } = await actor
    .openid_prepare_delegation(jwt, salt, publicKey)
    .then(throwCanisterError);

  console.log("✅ prepare_delegation:", {
    expiration,
    user_key: Buffer.from(user_key).toString("hex"),
  });

  const signedDelegation = await actor
    .openid_get_delegation(jwt, salt, publicKey, expiration)
    .then(throwCanisterError);

  console.log("✅ get_delegation:", signedDelegation);

  const transformed = transformSignedDelegation(signedDelegation);

  const chain = DelegationChain.fromDelegations(
    [transformed],
    new Uint8Array(user_key).buffer,
  );

  console.log("🔗 Delegation chain built", chain);

  return chain;
};
