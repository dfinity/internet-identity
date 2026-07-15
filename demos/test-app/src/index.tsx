import { VcFlowRequestWire } from "@dfinity/internet-identity-vc-api";
import { bytesToHex } from "@noble/hashes/utils";
import type { Identity, SignIdentity } from "@icp-sdk/core/agent";
import { Actor, HttpAgent } from "@icp-sdk/core/agent";
import {
  AttributesIdentity,
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
} from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";

import React, { useState } from "react";
import ReactDOM from "react-dom/client";

import { decodeJwt } from "jose";

import {
  authWithII,
  CertifiedAttribute,
  extractDelegation,
  Icrc3Attributes,
} from "./auth";
import { formatIcrc3Attributes } from "./icrc3";

import "./main.css";

const signInBtn = document.getElementById("signinBtn") as HTMLButtonElement;
const whoamiBtn = document.getElementById("whoamiBtn") as HTMLButtonElement;
const updateAlternativeOriginsBtn = document.getElementById(
  "updateNewAlternativeOrigins",
) as HTMLButtonElement;
const openIiWindowBtn = document.getElementById(
  "openIiWindowBtn",
) as HTMLButtonElement;
const closeIiWindowBtn = document.getElementById(
  "closeIIWindowBtn",
) as HTMLButtonElement;
const invalidDataBtn = document.getElementById(
  "invalidDataBtn",
) as HTMLButtonElement;
const incompleteMessageBtn = document.getElementById(
  "incompleteMessageBtn",
) as HTMLButtonElement;
const validMessageBtn = document.getElementById(
  "validMessageBtn",
) as HTMLButtonElement;
const customMessageEl = document.getElementById(
  "customMessage",
) as HTMLInputElement;
const customMessageBtn = document.getElementById(
  "customMessageBtn",
) as HTMLButtonElement;
const messagesEl = document.getElementById("messages") as HTMLElement;
const hostUrlEl = document.getElementById("hostUrl") as HTMLInputElement;
const whoAmIResponseEl = document.getElementById(
  "whoamiResponse",
) as HTMLDivElement;
const alternativeOriginsEl = document.getElementById(
  "alternativeOrigins",
) as HTMLDivElement;
const newAlternativeOriginsEl = document.getElementById(
  "newAlternativeOrigins",
) as HTMLInputElement;
const principalEl = document.getElementById("principal") as HTMLDivElement;
const authnMethodEl = document.querySelector(
  '[data-role="authn-method"]',
) as HTMLDivElement;
const delegationEl = document.getElementById("delegation") as HTMLPreElement;
const certifiedAttributesEl = document.getElementById(
  "certifiedAttributes",
) as HTMLPreElement;
const expirationEl = document.getElementById("expiration") as HTMLDivElement;
const iiUrlEl = document.getElementById("iiUrl") as HTMLInputElement;
const maxTimeToLiveEl = document.getElementById(
  "maxTimeToLive",
) as HTMLInputElement;
const derivationOriginEl = document.getElementById(
  "derivationOrigin",
) as HTMLInputElement;
const autoSelectionPrincipalEl = document.getElementById(
  "autoSelectionPrincipal",
) as HTMLInputElement;
const allowPinAuthenticationEl = document.getElementById(
  "allowPinAuthentication",
) as HTMLInputElement;
const useIcrc25El = document.getElementById("useIcrc25") as HTMLInputElement;
const useIcrc3AttributesEl = document.getElementById(
  "useIcrc3Attributes",
) as HTMLInputElement;
const icrc3NonceEl = document.getElementById("icrc3Nonce") as HTMLInputElement;
const requestAttributesEl = document.getElementById(
  "requestAttributes",
) as HTMLInputElement;
const icrc3AttributesEl = document.getElementById(
  "icrc3Attributes",
) as HTMLPreElement;
const icrc3AttributesDecodedEl = document.getElementById(
  "icrc3AttributesDecoded",
) as HTMLPreElement;
const sendAttributesBtn = document.getElementById(
  "sendAttributesBtn",
) as HTMLButtonElement;
const iiCanisterIdEl = document.getElementById(
  "iiCanisterId",
) as HTMLInputElement;
const canisterEchoedAttributesEl = document.getElementById(
  "canisterEchoedAttributes",
) as HTMLPreElement;
const canisterEchoedAttributesRawEl = document.getElementById(
  "canisterEchoedAttributesRaw",
) as HTMLPreElement;

// Push-notifications section (see index.html "Push Notifications" block).
const iiCanisterIdPushEl = document.getElementById(
  "iiCanisterIdPush",
) as HTMLInputElement;
const pushEnableBtn = document.getElementById(
  "pushEnableBtn",
) as HTMLButtonElement;
const pushNotifyBtn = document.getElementById(
  "pushNotifyBtn",
) as HTMLButtonElement;
const pushUnsubscribeBtn = document.getElementById(
  "pushUnsubscribeBtn",
) as HTMLButtonElement;
const pushStatusEl = document.getElementById("pushStatus") as HTMLDivElement;
const pushSubscriptionOutEl = document.getElementById(
  "pushSubscriptionOut",
) as HTMLPreElement;

let iiProtocolTestWindow: Window | undefined;

// The identity set by the authentication
let delegationIdentity: DelegationIdentity | undefined = undefined;

// The most recently received ICRC-3 attribute bundle, kept around so the
// "Send attributes to canister" button can wrap the delegation identity in
// `AttributesIdentity` and replay them against the test_app canister.
let latestIcrc3Attributes: Icrc3Attributes | undefined = undefined;

// The local, ephemeral key-pair
let localIdentity_: SignIdentity | undefined = undefined;

function getLocalIdentity(): SignIdentity {
  if (localIdentity_ === undefined) {
    localIdentity_ = Ed25519KeyIdentity.generate();
  }
  return localIdentity_;
}

const idlFactory = ({ IDL }: { IDL: any }) => {
  const HeaderField = IDL.Tuple(IDL.Text, IDL.Text);
  const HttpRequest = IDL.Record({
    url: IDL.Text,
    method: IDL.Text,
    body: IDL.Vec(IDL.Nat8),
    headers: IDL.Vec(HeaderField),
  });
  const HttpResponse = IDL.Record({
    body: IDL.Vec(IDL.Nat8),
    headers: IDL.Vec(HeaderField),
    status_code: IDL.Nat16,
  });
  const AlternativeOriginsMode = IDL.Variant({
    UncertifiedContent: IDL.Null,
    Redirect: IDL.Record({ location: IDL.Text }),
    CertifiedContent: IDL.Null,
  });
  const CallerAttributes = IDL.Record({
    signer: IDL.Opt(IDL.Principal),
    data: IDL.Vec(IDL.Nat8),
  });
  return IDL.Service({
    http_request: IDL.Func([HttpRequest], [HttpResponse], ["query"]),
    update_alternative_origins: IDL.Func(
      [IDL.Text, AlternativeOriginsMode],
      [],
      [],
    ),
    whoami: IDL.Func([], [IDL.Principal], ["query"]),
    caller_attributes: IDL.Func([], [CallerAttributes], []),
  });
};

// Hand-rolled IDL for the six II push methods, mirroring the six entries
// added on the .did file in this same PR. Kept local (no import from
// src/frontend/) so the test-app stays a standalone workspace.
const iiPushIdlFactory = ({ IDL }: { IDL: any }) => {
  const PushAlert = IDL.Record({
    hostname: IDL.Text,
    title: IDL.Text,
    body: IDL.Text,
    url: IDL.Opt(IDL.Text),
  });
  const OkErr = IDL.Variant({ Ok: IDL.Null, Err: IDL.Text });
  return IDL.Service({
    push_subscribe_device: IDL.Func(
      [IDL.Nat64, IDL.Text, IDL.Text, IDL.Vec(IDL.Nat8), IDL.Vec(IDL.Nat8)],
      [OkErr],
      [],
    ),
    push_unsubscribe_device: IDL.Func([IDL.Nat64, IDL.Text], [OkErr], []),
    notify_user: IDL.Func([IDL.Principal, PushAlert], [OkErr], []),
    push_vapid_public_key: IDL.Func([], [IDL.Vec(IDL.Nat8)], ["query"]),
  });
};

const updateDelegationView = ({
  authnMethod,
  identity,
  certifiedAttributes,
  icrc3Attributes,
}: {
  authnMethod?: string;
  identity: Identity;
  certifiedAttributes?: Record<string, CertifiedAttribute>;
  icrc3Attributes?: Icrc3Attributes;
}) => {
  principalEl.innerText = identity.getPrincipal().toText();

  if (authnMethod !== undefined) {
    authnMethodEl.innerText = authnMethod;
  }

  if (identity instanceof DelegationIdentity) {
    const delegation = identity.getDelegation();
    let jsonResult;
    try {
      // `toJSON` is failing due to `bytesToHex(delegation.publicKey)`
      // `bytesToHex` expects a Uint8Array and delegation.publicKey is a ArrayBuffer
      // jsonResult = delegation.toJSON();
      jsonResult = {
        delegations: delegation.delegations.map((signedDelegation) => {
          const { delegation, signature } = signedDelegation;
          const { targets } = delegation;
          return {
            delegation: {
              expiration: delegation.expiration.toString(16),
              pubkey: bytesToHex(delegation.pubkey),
              ...(targets && {
                targets: targets.map((t) => t.toHex()),
              }),
            },
            signature: bytesToHex(new Uint8Array(signature)),
          };
        }),
        publicKey: bytesToHex(new Uint8Array(delegation.publicKey)),
      };
    } catch (error) {
      console.error("toJSON error:", error);
      jsonResult = {
        error: "toJSON failed",
        message: error instanceof Error ? error.message : String(error),
      };
    }

    delegationEl.innerText = JSON.stringify(jsonResult, undefined, 2);

    // cannot use Math.min, as we deal with bigint here
    const nextExpiration = identity
      .getDelegation()
      .delegations.map((d) => d.delegation.expiration)
      .reduce((current, next) => (next < current ? next : current));
    expirationEl.innerText = (
      nextExpiration -
      BigInt(Date.now()) * BigInt(1000_000)
    ).toString();

    // Display certified attributes if available.
    if (certifiedAttributes !== undefined) {
      certifiedAttributesEl.innerText = Object.entries(certifiedAttributes)
        .sort(([keyA], [keyB]) => keyA.localeCompare(keyB))
        .map(([key, { value }]) => `${key}: ${new TextDecoder().decode(value)}`)
        .join("\n");
    } else {
      certifiedAttributesEl.innerText = "";
    }

    // Display ICRC-3 attributes if available, and stash the bundle so
    // the canister round-trip button can consume it.
    latestIcrc3Attributes = icrc3Attributes;
    canisterEchoedAttributesEl.innerText = "";
    if (icrc3Attributes !== undefined) {
      icrc3AttributesEl.innerText = JSON.stringify({
        // @ts-ignore Not known in TS types yet but supported in all browsers
        data: icrc3Attributes.data.toBase64(),
        // @ts-ignore Not known in TS types yet but supported in all browsers
        signature: icrc3Attributes.signature.toBase64(),
      });
      try {
        icrc3AttributesDecodedEl.innerText = formatIcrc3Attributes(
          icrc3Attributes.data,
        );
      } catch (err) {
        icrc3AttributesDecodedEl.innerText = `Failed to decode: ${
          err instanceof Error ? err.message : String(err)
        }`;
      }
    } else {
      icrc3AttributesEl.innerText = "";
      icrc3AttributesDecodedEl.innerText = "";
    }
  } else {
    delegationEl.innerText = "Current identity is not a DelegationIdentity";
    expirationEl.innerText = "N/A";
  }
};

const updateAlternativeOriginsView = async () => {
  const response = await fetch("/.well-known/ii-alternative-origins");
  alternativeOriginsEl.innerText = await response.text();
};

function addMessageElement({
  message,
  ty,
}: {
  message: unknown;
  ty: "received" | "sent";
}) {
  const messageContainer = document.createElement("div");
  messageContainer.classList.add("postMessage");
  const messageTitle = document.createElement("div");
  messageTitle.classList.add("postMessage-title");
  const messageContent = document.createElement("div");
  messageContent.innerText = JSON.stringify(message, (_, v) =>
    typeof v === "bigint" ? v.toString() : v,
  );
  if (ty === "received") {
    messageTitle.innerText = "Message Received";
    messageContainer.classList.add("received");
  } else {
    messageTitle.innerText = "Message Sent";
    messageContainer.classList.add("sent");
  }
  messageContainer.appendChild(messageTitle);
  messageContainer.appendChild(messageContent);
  messagesEl.appendChild(messageContainer);
}

window.addEventListener("message", (event) => {
  if (!event.source || event.source !== iiProtocolTestWindow) {
    return;
  }

  addMessageElement({ message: event.data, ty: "received" });

  if (event?.data?.kind !== "authorize-client-success") {
    return;
  }

  const delegations = event.data.delegations.map(extractDelegation);
  const delegationChain = DelegationChain.fromDelegations(
    delegations,
    event.data.userPublicKey.buffer,
  );
  updateDelegationView({
    identity: DelegationIdentity.fromDelegation(
      getLocalIdentity(),
      delegationChain,
    ),
  });
});

const readCanisterId = (): string => {
  const canIdEl = document.querySelector("[data-canister-id]") as HTMLElement;
  return canIdEl.dataset.canisterId!;
};

// Not entirely reliable, it fails on Android
function isTgInAppBrowser() {
  if (typeof window === "undefined") return false;

  // Telegram's webview bridge in mobile apps (Android & iOS)
  const hasBridge =
    typeof (window as any).TelegramWebviewProxy === "object" ||
    typeof (window as any).TelegramWebviewProxyProto === "object";

  // Useful but not guaranteed on iOS; often present on Android
  const ua = navigator.userAgent || "";
  const hasUA = /\bTelegram(?:Android|-Android)?\b/i.test(ua);

  return hasBridge || hasUA;
}

const init = async () => {
  const userAgentElement = document.getElementById("userAgent") as HTMLElement;
  userAgentElement.innerText = navigator.userAgent;
  const isTelegramElement = document.getElementById(
    "isTelegram",
  ) as HTMLElement;
  isTelegramElement.innerText = isTgInAppBrowser() ? "Yes" : "No";
  signInBtn.onclick = async () => {
    const maxTimeToLive_ = BigInt(maxTimeToLiveEl.value);
    // The default max TTL set in the @icp-sdk/auth/client library
    const authClientDefaultMaxTTL =
      /* hours */ BigInt(8) * /* nanoseconds */ BigInt(3_600_000_000_000);
    const maxTimeToLive =
      maxTimeToLive_ > BigInt(0) ? maxTimeToLive_ : authClientDefaultMaxTTL;
    const derivationOrigin =
      derivationOriginEl.value !== "" ? derivationOriginEl.value : undefined;
    const autoSelectionPrincipal =
      autoSelectionPrincipalEl.value !== ""
        ? autoSelectionPrincipalEl.value
        : undefined;

    const allowPinAuthentication = allowPinAuthenticationEl.checked
      ? undefined
      : false;

    try {
      const result = await authWithII({
        url: iiUrlEl.value,
        maxTimeToLive,
        derivationOrigin,
        allowPinAuthentication,
        sessionIdentity: getLocalIdentity(),
        autoSelectionPrincipal,
        useIcrc25: useIcrc25El.checked,
        useIcrc3Attributes: useIcrc3AttributesEl.checked,
        icrc3Nonce:
          icrc3NonceEl.value.trim() !== ""
            ? // @ts-ignore Not known in TS types yet but supported in all browsers
              Uint8Array.fromBase64(icrc3NonceEl.value.trim())
            : undefined,
        requestAttributes: requestAttributesEl.value
          .split("\n")
          .map((s) => s.trim())
          .filter((s) => s.length > 0),
      });
      delegationIdentity = result.identity;
      updateDelegationView({
        identity: delegationIdentity,
        authnMethod: result.authnMethod,
        certifiedAttributes: result.certifiedAttributes,
        icrc3Attributes: result.icrc3Attributes,
      });
    } catch (e) {
      showError(JSON.stringify(e));
    }
  };

  openIiWindowBtn.onclick = () => {
    // Open a new window with the IDP provider.
    if (iiProtocolTestWindow === undefined) {
      iiProtocolTestWindow =
        window.open(iiUrlEl.value + "#authorize", "iiWindow") ?? undefined;
    }
  };

  closeIiWindowBtn.onclick = () => {
    if (iiProtocolTestWindow) {
      iiProtocolTestWindow.close();
      iiProtocolTestWindow = undefined;
    }
  };

  invalidDataBtn.onclick = () => {
    if (!iiProtocolTestWindow) {
      alert("Open II tab first");
      return;
    }
    const invalidData = "some invalid data";
    addMessageElement({ message: invalidData, ty: "sent" });
    iiProtocolTestWindow.postMessage(invalidData, iiUrlEl.value);
  };

  incompleteMessageBtn.onclick = () => {
    if (!iiProtocolTestWindow) {
      alert("Open II tab first");
      return;
    }
    const incompleteMessage = { kind: "authorize-client" };
    addMessageElement({ message: incompleteMessage, ty: "sent" });
    iiProtocolTestWindow.postMessage(incompleteMessage, iiUrlEl.value);
  };

  validMessageBtn.onclick = () => {
    if (!iiProtocolTestWindow) {
      alert("Open II tab first");
      return;
    }
    let derivationOrigin =
      derivationOriginEl.value !== "" ? derivationOriginEl.value : undefined;
    let maxTimeToLive =
      BigInt(maxTimeToLiveEl.value) > BigInt(0)
        ? BigInt(maxTimeToLiveEl.value)
        : undefined;
    const validMessage = {
      kind: "authorize-client",
      sessionPublicKey: new Uint8Array(
        getLocalIdentity().getPublicKey().toDer(),
      ),
      derivationOrigin,
      maxTimeToLive,
    };
    addMessageElement({ message: validMessage, ty: "sent" });
    iiProtocolTestWindow.postMessage(validMessage, iiUrlEl.value);
  };

  customMessageBtn.onclick = () => {
    if (!iiProtocolTestWindow) {
      alert("Open II tab first");
      return;
    }
    let message = JSON.parse(customMessageEl.value);
    addMessageElement({ message, ty: "sent" });
    iiProtocolTestWindow.postMessage(message, iiUrlEl.value);
  };

  updateAlternativeOriginsBtn.onclick = async () => {
    const canisterId = Principal.fromText(readCanisterId());
    const httpAgent = await HttpAgent.create({
      host: hostUrlEl.value,
      shouldFetchRootKey: true,
    });
    const actor = Actor.createActor(idlFactory, {
      agent: httpAgent,
      canisterId,
    });
    const modeSelection = (
      document.querySelector(
        'input[name="alternativeOriginsMode"]:checked',
      ) as HTMLInputElement
    ).value;
    let mode:
      | { Redirect: { location: string } }
      | { CertifiedContent: null }
      | { UncertifiedContent: null } = { CertifiedContent: null };
    if (modeSelection === "uncertified") {
      mode = { UncertifiedContent: null };
    } else if (modeSelection === "redirect") {
      let location = (
        document.getElementById("redirectLocation") as HTMLInputElement
      ).value;
      mode = { Redirect: { location: location } };
    }
    await actor.update_alternative_origins(newAlternativeOriginsEl.value, mode);
    await updateAlternativeOriginsView();
  };
  await updateAlternativeOriginsView();
};

window.addEventListener("DOMContentLoaded", init);

sendAttributesBtn.addEventListener("click", async () => {
  if (delegationIdentity === undefined) {
    showError("Sign in first");
    return;
  }
  if (latestIcrc3Attributes === undefined) {
    showError("No ICRC-3 attribute bundle from the last sign-in");
    return;
  }
  const iiCanisterIdText = iiCanisterIdEl.value.trim();
  if (iiCanisterIdText === "") {
    showError("Set the II canister id (signer) first");
    return;
  }

  const canisterId = Principal.fromText(readCanisterId());
  const identity = new AttributesIdentity({
    inner: delegationIdentity,
    attributes: latestIcrc3Attributes,
    signer: { canisterId: Principal.fromText(iiCanisterIdText) },
  });
  const agent = await HttpAgent.create({
    host: hostUrlEl.value,
    identity,
    shouldFetchRootKey: true,
  });
  const actor = Actor.createActor(idlFactory, { agent, canisterId });

  canisterEchoedAttributesEl.innerText = "Loading...";
  canisterEchoedAttributesRawEl.innerText = "";
  try {
    const response = (await actor.caller_attributes()) as {
      signer: [] | [Principal];
      data: Uint8Array | number[];
    };
    const data =
      response.data instanceof Uint8Array
        ? response.data
        : new Uint8Array(response.data);
    canisterEchoedAttributesRawEl.innerText = JSON.stringify({
      signer: response.signer.length === 0 ? null : response.signer[0].toText(),
      // @ts-ignore Not known in TS types yet but supported in all browsers
      data: data.toBase64(),
    });
    const formatted = formatIcrc3Attributes(data);
    const signerText =
      response.signer.length === 0 ? "(none)" : response.signer[0].toText();
    canisterEchoedAttributesEl.innerText = `signer: ${signerText}\n${formatted}`;
  } catch (err) {
    canisterEchoedAttributesEl.innerText = `Failed: ${
      err instanceof Error ? err.message : String(err)
    }`;
  }
});

whoamiBtn.addEventListener("click", async () => {
  const canisterId = Principal.fromText(readCanisterId());
  const agent = await HttpAgent.create({
    host: hostUrlEl.value,
    identity: delegationIdentity,
    shouldFetchRootKey: true,
  });
  const actor = Actor.createActor(idlFactory, {
    agent,
    canisterId,
  });

  whoAmIResponseEl.innerText = "Loading...";

  actor
    .whoami()
    .then((principal: any) => {
      whoAmIResponseEl.innerText = principal.toText();
    })
    .catch((err) => {
      console.error("Failed to fetch whoami", err);
    });
});

// ---- Push notifications ---------------------------------------------
//
// The three buttons share an actor built from `delegationIdentity` (the
// delegation the user got from II for this app's origin). The delegation
// chain proves the identity is authorized to act as the anchor's per-app
// principal, so `push_subscribe_device` (authenticated as the anchor)
// and `notify_user` (`caller() == in_app_principal`) both succeed.

async function makeIiActor(): Promise<any> {
  if (!delegationIdentity) {
    throw new Error("Sign in first");
  }
  const raw = iiCanisterIdPushEl.value.trim();
  if (!raw) {
    throw new Error("Set the II canister ID");
  }
  const canisterId = Principal.fromText(raw);
  const agent = await HttpAgent.create({
    host: hostUrlEl.value,
    identity: delegationIdentity,
    shouldFetchRootKey: true,
  });
  return Actor.createActor(iiPushIdlFactory, { agent, canisterId });
}

function base64UrlDecode(s: string): Uint8Array {
  const pad = "=".repeat((4 - (s.length % 4)) % 4);
  const b64 = (s + pad).replace(/-/g, "+").replace(/_/g, "/");
  const bin = atob(b64);
  const out = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
  return out;
}

function setPushStatus(msg: string, kind: "ok" | "err") {
  pushStatusEl.textContent = msg;
  pushStatusEl.style.color = kind === "ok" ? "#1b5e20" : "#b71c1c";
}

// The anchor number must be threaded into `push_subscribe_device`.
// After sign-in the DelegationIdentity's inner principal is the app-
// specific principal, not the anchor — but the delegation chain carries
// the anchor's public key. We recover the anchor number from the
// `authnMethod` bundle stored during sign-in; if that's missing, ask.
function readAnchorForPush(): bigint {
  // The existing `principalEl` shows the app-scoped principal after
  // sign-in — not the anchor. This app doesn't currently persist the
  // raw anchor number, so we ask the user. Cheapest for a smoke test.
  const raw = window.prompt("Anchor number (Identity Number):");
  if (raw === null) {
    throw new Error("cancelled");
  }
  const trimmed = raw.trim();
  if (!/^\d+$/.test(trimmed)) {
    throw new Error("anchor must be a positive integer");
  }
  return BigInt(trimmed);
}

pushEnableBtn.addEventListener("click", async () => {
  try {
    setPushStatus("Requesting notification permission…", "ok");
    const perm = await Notification.requestPermission();
    if (perm !== "granted") {
      throw new Error(`notification permission: ${perm}`);
    }
    const iiActor = await makeIiActor();
    setPushStatus("Registering service worker…", "ok");
    const reg = await navigator.serviceWorker.register("/service-worker.js");
    await navigator.serviceWorker.ready;
    await reg.update();

    setPushStatus("Fetching VAPID key…", "ok");
    const vapidPubBytes = await iiActor.push_vapid_public_key();
    const vapidPub =
      vapidPubBytes instanceof Uint8Array
        ? vapidPubBytes
        : new Uint8Array(vapidPubBytes);

    setPushStatus("Subscribing with the relay…", "ok");
    const sub = await reg.pushManager.subscribe({
      userVisibleOnly: true,
      applicationServerKey: vapidPub,
    });
    const subJson = sub.toJSON() as {
      endpoint: string;
      keys: { p256dh: string; auth: string };
    };
    pushSubscriptionOutEl.textContent = JSON.stringify(subJson, null, 2);

    const anchor = readAnchorForPush();
    const p256dh = base64UrlDecode(subJson.keys.p256dh);
    const auth = base64UrlDecode(subJson.keys.auth);

    setPushStatus("Registering on II…", "ok");
    const res = await iiActor.push_subscribe_device(
      anchor,
      window.location.origin,
      sub.endpoint,
      p256dh,
      auth,
    );
    if ("Err" in res) throw new Error(res.Err);
    setPushStatus("Subscribed", "ok");
  } catch (err) {
    setPushStatus(`Subscribe failed: ${(err as Error).message}`, "err");
  }
});

pushNotifyBtn.addEventListener("click", async () => {
  try {
    if (!delegationIdentity) throw new Error("Sign in first");
    const iiActor = await makeIiActor();
    const res = await iiActor.notify_user(delegationIdentity.getPrincipal(), {
      hostname: window.location.host,
      title: "Hello from test-app",
      body: "Push notifications work!",
      url: [],
    });
    if ("Err" in res) throw new Error(res.Err);
    setPushStatus("notify_user Ok — watch the OS tray", "ok");
  } catch (err) {
    setPushStatus(`notify_user failed: ${(err as Error).message}`, "err");
  }
});

pushUnsubscribeBtn.addEventListener("click", async () => {
  try {
    const iiActor = await makeIiActor();
    const anchor = readAnchorForPush();
    const reg = await navigator.serviceWorker.getRegistration();
    const sub = await reg?.pushManager.getSubscription();
    if (sub) await sub.unsubscribe();
    const res = await iiActor.push_unsubscribe_device(
      anchor,
      window.location.origin,
    );
    if ("Err" in res) throw new Error(res.Err);
    pushSubscriptionOutEl.textContent = "—";
    setPushStatus("Unsubscribed", "ok");
  } catch (err) {
    setPushStatus(`Unsubscribe failed: ${(err as Error).message}`, "err");
  }
});

const showError = (err: string) => {
  alert(err);
};

/* The various kinds of specs the issuer canister can issue */
const credentialSpecs = {
  employee: {
    credentialType: "VerifiedEmployee",
    arguments: { employerName: "DFINITY Foundation" },
  },
  grad: {
    credentialType: "UniversityDegreeCredential",
    arguments: { institutionName: "DFINITY College of Engineering" },
  },
  adult: {
    credentialType: "VerifiedAdult",
    arguments: { minAge: 18 },
  },
} as const;

type CredType = keyof typeof credentialSpecs;

/* The latest opts picked by the user. A global, because the flow is very much async */
let latestOpts:
  | undefined
  | {
      issuerOrigin: string;
      issuerCanisterId: string;
      derivationOrigin?: string;
      credTy: CredType;
      flowId: number;
      win: Window;
    };

/* Callback for setting the latest received presentation. Set by React when it boots. */
let setLatestPresentation: (pres: {
  alias: string;
  credential: string;
}) => void = (_) => {
  /* */
};

/* Handler for an II message telling us the window is ready */
function handleFlowReady(evnt: MessageEvent) {
  if (evnt.data?.method !== "vc-flow-ready") {
    return;
  }

  const opts = latestOpts;

  if (opts === undefined) {
    return showError(
      "Unexpected: received OK from IDP but this test app is not ready",
    );
  }

  const principal = principalEl.innerText;
  if (principal === "") {
    return showError("Principal is not set, please authenticate");
  }

  try {
    Principal.fromText(principal) satisfies Principal;
  } catch {
    return showError(`"${principal}" is not a principal`);
  }

  const req: VcFlowRequestWire = {
    id: opts.flowId.toString(),
    jsonrpc: "2.0",
    method: "request_credential",
    params: {
      issuer: {
        origin: opts.issuerOrigin,
        canisterId: opts.issuerCanisterId,
      },
      credentialSpec: credentialSpecs[opts.credTy],
      credentialSubject: principal,
      derivationOrigin: opts.derivationOrigin,
    },
  };

  // register a handler for the "done" message, kickstart the flow and then
  // unregister ourselves
  try {
    window.addEventListener("message", handleFlowFinished);
    evnt.source?.postMessage(req, { targetOrigin: evnt.origin });
  } finally {
    window.removeEventListener("message", handleFlowReady);
  }
}

function handleFlowFinished(evnt: MessageEvent) {
  if (latestOpts === undefined) {
    // no inflight requests, so we don't expect a response.
    return;
  }

  if (evnt.data?.id.toString() !== latestOpts.flowId.toString()) {
    // If this is not a response to a flow we started, ignore it
    return;
  }

  try {
    // Make the presentation presentable
    const verifiablePresentation = evnt.data?.result?.verifiablePresentation;
    if (verifiablePresentation === undefined) {
      return showError("No presentation");
    }

    const ver = decodeJwt(verifiablePresentation) as any;
    const creds = ver.vp.verifiableCredential;
    const [alias, credential] = creds.map((cred: string) =>
      JSON.stringify(decodeJwt(cred), null, 2),
    );

    setLatestPresentation({ alias, credential });
    latestOpts?.win.close();
  } finally {
    window.removeEventListener("message", handleFlowFinished);
  }
}

const App = () => {
  // The URL used for connecting to the issuer
  const [issuerUrl, setIssuerUrl] = useState<string>(
    "http://issuer.localhost:5173",
  );

  const [issuerCanisterId, setIssuerCanisterId] = useState<string>("");

  // Alternative origin for the RP, if any
  const [derivationOrigin, setDerivationOrigin] = useState<string>("");

  // Continuously incrementing flow IDs used in the JSON RPC messages
  const [nextFlowId, setNextFlowId] = useState(0);

  // Latest presentation generated by a VC flow
  const [latestPresentation, setLatestPresentation_] = useState<
    undefined | { alias: string; credential: string }
  >(undefined);
  setLatestPresentation = setLatestPresentation_;

  // Kickstart the VC flow
  const startVcFlow = (credTy: CredType) => {
    const urlRaw = iiUrlEl.value;

    const urlParsed = new URL(urlRaw);
    urlParsed.pathname = "vc-flow/";

    const iiWindow = window.open(urlParsed.toString());
    if (iiWindow === undefined || iiWindow === null) {
      return showError("Could not open window");
    }

    const flowId = nextFlowId;
    setNextFlowId(flowId + 1);

    latestOpts = {
      flowId,
      credTy,
      issuerOrigin: new URL(issuerUrl).origin,
      issuerCanisterId,
      derivationOrigin: derivationOrigin !== "" ? derivationOrigin : undefined,
      win: iiWindow,
    };

    window.addEventListener("message", handleFlowReady);
  };

  return (
    <>
      <h1>Verifiable Credentials</h1>
      <label>
        Issuer URL:
        <input
          data-role="issuer-url"
          type="text"
          value={issuerUrl}
          onChange={(evt) => setIssuerUrl(evt.target.value)}
        />
      </label>
      <label>
        Issuer canister Id:
        <input
          data-role="issuer-canister-id"
          type="text"
          value={issuerCanisterId}
          onChange={(evt) => setIssuerCanisterId(evt.target.value)}
        />
      </label>
      <label>
        Alternative Derivation Origin:
        <input
          data-role="derivation-origin-rp"
          type="text"
          placeholder="(use default)"
          onChange={(evt) => setDerivationOrigin(evt.target.value)}
        />
      </label>

      <button
        data-action="verify-employee"
        onClick={() => startVcFlow("employee")}
      >
        Verify Employee Credential
      </button>
      <button data-action="verify-grad" onClick={() => startVcFlow("grad")}>
        Verify Graduate Credential
      </button>
      <button data-action="verify-adult" onClick={() => startVcFlow("adult")}>
        Verify Adult Person Credential
      </button>

      {latestPresentation && (
        <>
          <pre data-role="presentation-alias">{latestPresentation.alias}</pre>
          <pre data-role="presentation-credential">
            {latestPresentation.credential}
          </pre>
        </>
      )}
    </>
  );
};

ReactDOM.createRoot(document.getElementById("root-vc-flow")!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
);
