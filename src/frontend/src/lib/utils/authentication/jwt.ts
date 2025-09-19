import { Principal } from "@dfinity/principal";
import { Actor, HttpAgent } from "@dfinity/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import {
  isCanisterError,
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";
import {
  Delegation,
  DelegationChain,
  DelegationIdentity,
  ECDSAKeyIdentity,
} from "@dfinity/identity";
import { Session } from "$lib/stores/session.store";
import { restoreECDSAIdentity } from "../restoreECDSAIdentity";
import { decodeJWT } from "../openID";
import { IDL } from "@dfinity/candid";

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
  intermediateIdentity,
  appIdentity,
}: {
  canisterId: Principal;
  jwt: string;
  salt: Uint8Array;
  intermediateIdentity: ECDSAKeyIdentity;
  appIdentity: ECDSAKeyIdentity;
}): Promise<{
  identity: DelegationIdentity;
  identityNumber: bigint;
}> => {
  console.log("📨 Starting redirect callback authentication");
  console.log("🎟️ JWT (first 50 chars):", jwt.slice(0, 50), "...");
  console.log("🧂 Salt (hex):", Buffer.from(salt).toString("hex"));
  console.log(
    "🔑 II Public Key (DER hex):",
    Buffer.from(intermediateIdentity.getPublicKey().toDer()).toString("hex"),
  );

  const publicKey = new Uint8Array(intermediateIdentity.getPublicKey().toDer());
  const agent = new HttpAgent({ identity: intermediateIdentity });
  if (import.meta.env.DEV) await agent.fetchRootKey();

  const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
    agent,
    canisterId,
  });

  console.log("test1");
  const {
    anchor_number: identityNumber,
    expiration,
    user_key,
  } = await actor
    .openid_prepare_delegation(jwt, salt, publicKey)
    .then(throwCanisterError);

  console.log("test2");

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

  // First: use II's chain to delegate to intermediateIdentity
  const intermediateDelegationIdentity = DelegationIdentity.fromDelegation(
    intermediateIdentity,
    chain,
  );

  const value = intermediateDelegationIdentity.getPrincipal();

  value.toString();
  await testIdentity(
    "http://127.0.0.1:4943",
    "u6s2n-gx777-77774-qaaba-cai",
    "whoami",
    intermediateDelegationIdentity,
  );

  await testIdentity(
    "http://127.0.0.1:4943",
    "u6s2n-gx777-77774-qaaba-cai",
    "whoami",
  );

  chain.delegations.forEach((d, i) => {
    console.log(`🔗 Chain level ${i}:`, {
      pubkey: Buffer.from(d.delegation.pubkey).toString("hex"),
      expiration: d.delegation.expiration.toString(),
    });
  });
  console.log(
    "➕ Adding delegation to:",
    Buffer.from(appIdentity.getPublicKey().toDer()).toString("hex"),
  );

  const newExpiration = BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000); // 1 hour
  const finalDelegation = new Delegation(
    appIdentity.getPublicKey().toDer(),
    newExpiration,
  );

  const signedFinalDelegation = await intermediateIdentity.sign(
    finalDelegation.toCBOR(),
  );

  const signedDelegation2 = {
    delegation: finalDelegation,
    signature: signedFinalDelegation,
  };

  const extendedChain = DelegationChain.fromDelegations(
    [...chain.delegations, signedDelegation2],
    chain.publicKey,
  );

  const identity = DelegationIdentity.fromDelegation(
    appIdentity,
    extendedChain,
  );

  console.log(
    "🗝️ Root public key (chain):",
    Buffer.from(chain.publicKey).toString("hex"),
  );

  console.log(
    "🧑 Intermediate principal:",
    intermediateDelegationIdentity.getPrincipal().toText(),
  );
  console.log("📱 App principal:", identity.getPrincipal().toText());
  console.log(
    "🧾 Delegation chain JSON:",
    JSON.stringify(identity.getDelegation()?.toJSON(), null, 2),
  );

  // run tests again

  value.toString();
  await testIdentity(
    "http://127.0.0.1:4943",
    "u6s2n-gx777-77774-qaaba-cai",
    "whoami",
    identity,
  );

  await testIdentity(
    "http://127.0.0.1:4943",
    "u6s2n-gx777-77774-qaaba-cai",
    "whoami",
  );

  return { identity, identityNumber };
};

export const testIdentity = async (
  host: string,
  canisterId: string,
  methodName: string,
  identity?: DelegationIdentity,
) => {
  try {
    const agent2 = await HttpAgent.create({
      host,
      identity,
    });
    await agent2.fetchRootKey();

    const arg = IDL.encode([], []);
    const result = await agent2.query(canisterId, {
      methodName,
      arg,
    });

    console.log(
      "Backend whoami:",
      JSON.stringify(result, (_, v) =>
        typeof v === "bigint" ? v.toString() : v,
      ),
    );
  } catch (err) {
    console.error("❌ testIdentity failed:", err);
    throw err;
  }
};
