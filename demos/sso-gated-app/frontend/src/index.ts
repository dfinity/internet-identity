import { AuthClient } from "@icp-sdk/auth/client";
import { Actor, HttpAgent } from "@icp-sdk/core/agent";
import { AttributesIdentity } from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import { idlFactory } from "./idl";

// ── Tiny DOM helpers ────────────────────────────────────────────────
const $ = <T extends HTMLElement>(id: string): T =>
  document.getElementById(id) as T;
const val = (id: string): string => $<HTMLInputElement>(id).value.trim();
const log = (msg: string): void => {
  $("output").textContent = msg;
};

// The inner delegation identity from the last successful sign-in. The
// SSO session is recorded canister-side under this principal, so later
// protected calls don't need to re-present the attribute bundle.
let loggedIn: Awaited<ReturnType<AuthClient["signIn"]>> | undefined;

const makeActor = async (identity?: unknown) => {
  const agent = await HttpAgent.create({
    host: val("host"),
    identity: identity as never,
    shouldFetchRootKey: $<HTMLInputElement>("fetchRootKey").checked,
  });
  return Actor.createActor(idlFactory, {
    agent,
    canisterId: Principal.fromText(val("appCanisterId")),
  });
};

// ── Sign in via org SSO, request the sso:<domain> bundle, verify ─────
const signInAndVerify = async (): Promise<void> => {
  try {
    log("Fetching nonce and opening Internet Identity…");
    const domain = val("ssoDomain");

    // 1. Anonymous handle, used only to mint a single-use nonce.
    const anonymousActor = await makeActor();
    const noncePromise = (
      anonymousActor._internet_identity_sign_in_start() as Promise<
        Uint8Array | number[]
      >
    ).then((n) => (n instanceof Uint8Array ? n : new Uint8Array(n)));

    // 2. Sign-in and the attribute request run together, so the user
    //    sees a single II prompt. Request the org-membership attribute
    //    for this app under the SSO domain.
    const authClient = new AuthClient({ identityProvider: val("iiUrl") });
    const signInPromise = authClient.signIn();
    const attributesPromise = authClient.requestAttributes({
      keys: [`sso:${domain}:name`, `sso:${domain}:email`],
      nonce: noncePromise,
    });

    const identity = await signInPromise;
    const attributes = await attributesPromise;
    loggedIn = identity;

    // 3. Replay the bundle wrapped in an AttributesIdentity so it
    //    travels as sender_info; the signer is the trusted II canister.
    const verifiedIdentity = new AttributesIdentity({
      inner: identity,
      attributes,
      signer: { canisterId: Principal.fromText(val("signerCanisterId")) },
    });
    const verifiedAgent = await HttpAgent.create({
      host: val("host"),
      identity: verifiedIdentity,
      shouldFetchRootKey: $<HTMLInputElement>("fetchRootKey").checked,
    });
    const verifiedActor = Actor.createActor(idlFactory, {
      agent: verifiedAgent,
      canisterId: Principal.fromText(val("appCanisterId")),
    });

    const result = (await verifiedActor._internet_identity_sign_in_finish()) as
      | { ok: null }
      | { err: unknown };

    if ("ok" in result) {
      log(
        `sign-in-finish: #ok — bundle verified.\n` +
          `You can now call the protected resource below.`,
      );
    } else {
      log(
        `sign-in-finish: #err\n${JSON.stringify(result.err, bigintReplacer, 2)}`,
      );
    }
  } catch (err) {
    log(`Sign-in failed: ${err instanceof Error ? err.message : String(err)}`);
  }
};

// ── Call the gated resource as the logged-in principal ───────────────
const callProtected = async (): Promise<void> => {
  if (loggedIn === undefined) {
    log("Sign in first.");
    return;
  }
  try {
    const actor = await makeActor(loggedIn);
    const [resource, session, principal] = await Promise.all([
      actor.getProtectedResource() as Promise<
        { ok: string } | { err: Record<string, null> }
      >,
      actor.sessionInfo() as Promise<unknown>,
      actor.whoami() as Promise<Principal>,
    ]);

    const verdict =
      "ok" in resource
        ? `✅ ALLOWED — ${resource.ok}`
        : `⛔ DENIED — ${Object.keys(resource.err)[0]}`;

    log(
      `${verdict}\n\n` +
        `principal: ${principal.toString()}\n` +
        `session:   ${JSON.stringify(session, bigintReplacer, 2)}`,
    );
  } catch (err) {
    log(`Call failed: ${err instanceof Error ? err.message : String(err)}`);
  }
};

// JSON.stringify can't serialize the bigint in `verifiedAtNs`.
const bigintReplacer = (_key: string, value: unknown): unknown =>
  typeof value === "bigint" ? value.toString() : value;

$("signInBtn").addEventListener("click", () => void signInAndVerify());
$("callBtn").addEventListener("click", () => void callProtected());
