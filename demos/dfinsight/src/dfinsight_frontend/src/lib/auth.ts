import { AuthClient } from "@icp-sdk/auth/client";
import { AnonymousIdentity } from "@icp-sdk/core/agent";
import type { Identity } from "@icp-sdk/core/agent";
import { AttributesIdentity } from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";

import { II_URL, II_CANISTER_ID } from "./config";
import { makeAnonymousBackend, makeBackend } from "./backend";
import type { DfinsightBackend } from "./declarations/dfinsight_backend.types";

export type Session =
  | { kind: "anon"; identity: Identity; backend: DfinsightBackend }
  | {
      kind: "admin";
      identity: Identity;
      backend: DfinsightBackend;
      // Inner principal — what the backend sees as `caller`.
      principal: string;
      // Verified name from the SSO attribute bundle.
      name: string;
      // Server-side admin session expiry (nanoseconds since epoch).
      expiresAtNs: bigint;
    };

export class AdminSignInError extends Error {
  constructor(public adminError: import("./declarations/dfinsight_backend.types").AdminError) {
    super("admin sign-in failed");
  }
}

let authClient: AuthClient | null = null;

async function ensureClient(): Promise<AuthClient> {
  if (!authClient) {
    authClient = new AuthClient({ identityProvider: II_URL });
  }
  return authClient;
}

/// 1-click anonymous-ish sign-in via the DFINITY SSO discovery flow.
/// The backend sees a stable principal (so we can dedupe upvotes and
/// enforce the 24h post limit) but never the user's name or email.
export async function signInAnonymous(): Promise<Session> {
  const client = await ensureClient();
  await client.signIn();
  const identity = await client.getIdentity();
  const backend = await makeBackend(identity);
  return { kind: "anon", identity, backend };
}

/// 1-click admin sign-in. Same SSO flow, but we *also* request the
/// `sso:dfinity.org:name` attribute and wrap the session identity in
/// `AttributesIdentity` so the bundle rides on every ingress message
/// (where the canister picks it up via `mo:core/CallerAttributes`).
export async function signInAdmin(): Promise<Session> {
  const client = await ensureClient();

  // 1. Pull a 32-byte canister-issued nonce (Authorization tier requires
  //    `implicit:nonce` to match something the canister has already
  //    committed to).
  const bootstrap = await makeAnonymousBackend();
  const nonceRaw = await bootstrap.generate_nonce();
  const nonce = nonceRaw instanceof Uint8Array ? nonceRaw : new Uint8Array(nonceRaw);

  // 2. signIn + requestAttributes in one popup. `Promise.all` rather
  //    than two awaits — if signIn rejects, we still observe the
  //    requestAttributes settlement.
  const signInPromise = client.signIn();
  const attributesPromise = client.requestAttributes({
    keys: ["sso:dfinity.org:name"],
    nonce,
  });
  const [, { data, signature }] = await Promise.all([
    signInPromise,
    attributesPromise,
  ]);

  // 3. Wrap with AttributesIdentity. Without this the bundle never
  //    reaches the canister and `II.verify<system>` returns
  //    `#NoAttributes`.
  const inner = await client.getIdentity();
  const identity = new AttributesIdentity({
    inner,
    attributes: { data, signature },
    signer: { canisterId: Principal.fromText(II_CANISTER_ID) },
  });

  const backend = await makeBackend(identity);

  // 4. Burn the bundle on the backend's `establishAdminSession`. This
  //    is what verifies the caller is *actually* on the admin allowlist
  //    — the SSO popup itself only proves they have an `sso:dfinity.org`
  //    identity and a `name` attribute. If the name isn't in the list,
  //    the backend returns `#NotAdmin { name; admins }` and we surface
  //    that to the UI via `AdminSignInError`.
  const res = await backend.establishAdminSession();
  if ("err" in res) throw new AdminSignInError(res.err);

  return {
    kind: "admin",
    identity,
    backend,
    principal: inner.getPrincipal().toText(),
    name: res.ok.name,
    expiresAtNs: res.ok.expiresAt,
  };
}

export async function signOut(): Promise<void> {
  const client = await ensureClient();
  await client.logout();
}

/// Returns the cached session if still valid, or null. Useful for
/// hydrating the UI on page load without forcing another sign-in.
export async function restoreAnonSession(): Promise<Session | null> {
  const client = await ensureClient();
  if (!client.isAuthenticated()) return null;
  const identity = await client.getIdentity();
  // We can't tell from the cached identity alone whether the user
  // originally signed in for the admin path or the anon path —
  // attribute identities aren't restored from storage in @icp-sdk/auth.
  // Treat restored sessions as anon; the admin page re-runs the full
  // attribute flow on demand.
  if (identity.getPrincipal().isAnonymous()) return null;
  const backend = await makeBackend(identity);
  return { kind: "anon", identity, backend };
}

export async function makePublicBackend(): Promise<DfinsightBackend> {
  // Publicly-readable methods (`listAdmins`) work fine as anonymous.
  return makeBackend(new AnonymousIdentity());
}
