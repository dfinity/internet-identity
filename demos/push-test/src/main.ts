// Driver for the push-notifications smoke test.
//
// Flow the buttons execute end-to-end:
//   1. Sign in with II (opens a popup on the configured II URL).
//   2. Register the SW under this origin, fetch VAPID pub via
//      `push_vapid_public_key`, call `pushManager.subscribe()`, then
//      `push_subscribe_device` to record the subscription on II.
//   3. `push_grant_consent` — writes the reverse index used by notify_user.
//   4. `notify_user` — encrypts, spawns the outcall, browser SW displays.

import { Actor, HttpAgent, type Identity } from "@icp-sdk/core/agent";
import { DelegationIdentity, Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import { AuthClient } from "@icp-sdk/auth/client";
import { IDL } from "@icp-sdk/core/candid";

// The @icp-sdk/auth `AuthClient` constructor and its `signIn` method
// return an `Identity`, but that identity is always a `DelegationIdentity`
// when the underlying ICRC-25 flow succeeded — see the pattern in
// `demos/test-app/src/auth.ts`.

// ---- II IDL (hand-declared: only the 6 push methods) ----------------

const PushAlertIDL = IDL.Record({
  hostname: IDL.Text,
  title: IDL.Text,
  body: IDL.Text,
  url: IDL.Opt(IDL.Text),
});

const OkErrIDL = IDL.Variant({ Ok: IDL.Null, Err: IDL.Text });

const idlFactory = () =>
  IDL.Service({
    push_subscribe_device: IDL.Func(
      [IDL.Nat64, IDL.Text, IDL.Text, IDL.Vec(IDL.Nat8), IDL.Vec(IDL.Nat8)],
      [OkErrIDL],
      [],
    ),
    push_unsubscribe_device: IDL.Func(
      [IDL.Nat64, IDL.Text],
      [OkErrIDL],
      [],
    ),
    push_grant_consent: IDL.Func([IDL.Nat64, IDL.Text], [OkErrIDL], []),
    push_revoke_consent: IDL.Func([IDL.Nat64, IDL.Text], [OkErrIDL], []),
    notify_user: IDL.Func([IDL.Principal, PushAlertIDL], [OkErrIDL], []),
    push_vapid_public_key: IDL.Func([], [IDL.Vec(IDL.Nat8)], ["query"]),
  });

interface IIActor {
  push_subscribe_device: (
    anchor: bigint,
    origin: string,
    endpoint: string,
    p256dh: Uint8Array,
    auth: Uint8Array,
  ) => Promise<{ Ok: null } | { Err: string }>;
  push_unsubscribe_device: (
    anchor: bigint,
    origin: string,
  ) => Promise<{ Ok: null } | { Err: string }>;
  push_grant_consent: (
    anchor: bigint,
    origin: string,
  ) => Promise<{ Ok: null } | { Err: string }>;
  push_revoke_consent: (
    anchor: bigint,
    origin: string,
  ) => Promise<{ Ok: null } | { Err: string }>;
  notify_user: (
    inAppPrincipal: Principal,
    alert: {
      hostname: string;
      title: string;
      body: string;
      url: [] | [string];
    },
  ) => Promise<{ Ok: null } | { Err: string }>;
  push_vapid_public_key: () => Promise<Uint8Array | number[]>;
}

// ---- DOM refs ---------------------------------------------------------

const $ = <T extends HTMLElement>(id: string): T =>
  document.getElementById(id) as T;

const iiUrlEl = $<HTMLInputElement>("iiUrl");
const iiCanisterIdEl = $<HTMLInputElement>("iiCanisterId");
const anchorEl = $<HTMLInputElement>("anchorNumber");
const alertTitleEl = $<HTMLInputElement>("alertTitle");
const alertBodyEl = $<HTMLInputElement>("alertBody");
const principalOut = $<HTMLPreElement>("principalOut");
const subscriptionOut = $<HTMLPreElement>("subscriptionOut");

const signInBtn = $<HTMLButtonElement>("signInBtn");
const subscribeBtn = $<HTMLButtonElement>("subscribeBtn");
const grantConsentBtn = $<HTMLButtonElement>("grantConsentBtn");
const revokeConsentBtn = $<HTMLButtonElement>("revokeConsentBtn");
const notifyBtn = $<HTMLButtonElement>("notifyBtn");

// Per-section status divs, so users can see a running log per step.
const signInStatus = $<HTMLDivElement>("signInStatus");
const subscribeStatus = $<HTMLDivElement>("subscribeStatus");
const consentStatus = $<HTMLDivElement>("consentStatus");
const notifyStatus = $<HTMLDivElement>("notifyStatus");

// ---- Shared state -----------------------------------------------------

let authClient: AuthClient | null = null;
let identity: Identity | null = null;
let iiActor: IIActor | null = null;

// ---- Helpers ----------------------------------------------------------

function setStatus(el: HTMLDivElement, msg: string, kind: "ok" | "err") {
  el.className = `status ${kind}`;
  el.textContent = msg;
}

function readAnchor(): bigint {
  const raw = anchorEl.value.trim();
  if (!/^\d+$/.test(raw)) {
    throw new Error("Anchor number must be a positive integer");
  }
  return BigInt(raw);
}

function readCanisterId(): Principal {
  const raw = iiCanisterIdEl.value.trim();
  if (!raw) {
    throw new Error("II canister ID is required");
  }
  return Principal.fromText(raw);
}

async function makeActor(id: Identity): Promise<IIActor> {
  // `shouldFetchRootKey: true` so this works against a local dfx replica.
  // For a production mainnet canister flip it to false — the boundary
  // node's root key is baked into the agent.
  const agent = await HttpAgent.create({
    host: iiUrlEl.value,
    identity: id,
    shouldFetchRootKey: true,
  });
  return Actor.createActor(idlFactory, {
    agent,
    canisterId: readCanisterId(),
  }) as unknown as IIActor;
}

function toUint8Array(bytesLike: Uint8Array | number[]): Uint8Array {
  return bytesLike instanceof Uint8Array
    ? bytesLike
    : new Uint8Array(bytesLike);
}

function throwErr(res: { Ok: null } | { Err: string }): asserts res is { Ok: null } {
  if ("Err" in res) {
    throw new Error(res.Err);
  }
}

// ---- Step 1: sign in --------------------------------------------------

signInBtn.addEventListener("click", async () => {
  try {
    // Fresh session identity per sign-in — the browser's Ed25519 keypair
    // that the II delegation later delegates *to*. Not persisted; sign-in
    // again → new session identity → new principal.
    const sessionIdentity = Ed25519KeyIdentity.generate();
    authClient = new AuthClient({
      identity: sessionIdentity,
      identityProvider: iiUrlEl.value,
      idleOptions: { disableIdle: true },
    });
    // Long TTL so the smoke session doesn't expire mid-test.
    const maxTimeToLive = BigInt(4 * 60 * 60) * BigInt(1_000_000_000);
    const signed = await authClient.signIn({ maxTimeToLive });
    if (!(signed instanceof DelegationIdentity)) {
      throw new Error(
        `expected DelegationIdentity, got ${signed.constructor.name}`,
      );
    }
    identity = signed;
    iiActor = await makeActor(identity);
    principalOut.textContent = identity.getPrincipal().toText();
    setStatus(signInStatus, "Signed in", "ok");
  } catch (err) {
    setStatus(signInStatus, `Sign-in failed: ${(err as Error).message}`, "err");
  }
});

// ---- Step 2: subscribe -----------------------------------------------

subscribeBtn.addEventListener("click", async () => {
  try {
    if (!iiActor) throw new Error("Sign in first");
    // Ask permission up front so the subscribe() call doesn't silently
    // stall — Chrome only shows the permission prompt on user gesture.
    const perm = await Notification.requestPermission();
    if (perm !== "granted") {
      throw new Error(`Notification permission: ${perm}`);
    }

    const reg = await navigator.serviceWorker.register("/service-worker.js");
    // Wait until the SW is active — subscribe() rejects otherwise.
    await navigator.serviceWorker.ready;
    // `reg.update()` is defensive: forces the newest SW code even if
    // the cached one is still valid.
    await reg.update();

    const vapidPubBytes = toUint8Array(await iiActor.push_vapid_public_key());
    const sub = await reg.pushManager.subscribe({
      userVisibleOnly: true,
      applicationServerKey: vapidPubBytes,
    });
    const subJson = sub.toJSON() as {
      endpoint: string;
      keys: { p256dh: string; auth: string };
    };
    const p256dh = toUint8Array(base64UrlDecode(subJson.keys.p256dh));
    const auth = toUint8Array(base64UrlDecode(subJson.keys.auth));

    subscriptionOut.textContent = JSON.stringify(subJson, null, 2);

    const anchor = readAnchor();
    const res = await iiActor.push_subscribe_device(
      anchor,
      window.location.origin,
      sub.endpoint,
      p256dh,
      auth,
    );
    throwErr(res);
    setStatus(subscribeStatus, "Subscribed", "ok");
  } catch (err) {
    setStatus(
      subscribeStatus,
      `Subscribe failed: ${(err as Error).message}`,
      "err",
    );
  }
});

// ---- Step 3: consent -------------------------------------------------

grantConsentBtn.addEventListener("click", async () => {
  try {
    if (!iiActor) throw new Error("Sign in first");
    const res = await iiActor.push_grant_consent(
      readAnchor(),
      window.location.origin,
    );
    throwErr(res);
    setStatus(consentStatus, "Consent granted", "ok");
  } catch (err) {
    setStatus(
      consentStatus,
      `Grant failed: ${(err as Error).message}`,
      "err",
    );
  }
});

revokeConsentBtn.addEventListener("click", async () => {
  try {
    if (!iiActor) throw new Error("Sign in first");
    const res = await iiActor.push_revoke_consent(
      readAnchor(),
      window.location.origin,
    );
    throwErr(res);
    setStatus(consentStatus, "Consent revoked", "ok");
  } catch (err) {
    setStatus(
      consentStatus,
      `Revoke failed: ${(err as Error).message}`,
      "err",
    );
  }
});

// ---- Step 4: notify --------------------------------------------------

notifyBtn.addEventListener("click", async () => {
  try {
    if (!iiActor || !identity) throw new Error("Sign in first");
    const res = await iiActor.notify_user(identity.getPrincipal(), {
      hostname: window.location.host,
      title: alertTitleEl.value || "Notification",
      body: alertBodyEl.value || "",
      url: [],
    });
    throwErr(res);
    setStatus(
      notifyStatus,
      "notify_user Ok — watch your notification tray",
      "ok",
    );
  } catch (err) {
    setStatus(
      notifyStatus,
      `notify_user failed: ${(err as Error).message}`,
      "err",
    );
  }
});

// ---- Utilities --------------------------------------------------------

function base64UrlDecode(s: string): Uint8Array {
  const pad = "=".repeat((4 - (s.length % 4)) % 4);
  const b64 = (s + pad).replace(/-/g, "+").replace(/_/g, "/");
  const bin = atob(b64);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) {
    out[i] = bin.charCodeAt(i);
  }
  return out;
}

