import { VcFlowRequestWire } from "@dfinity/internet-identity-vc-api";

import type { Identity, SignIdentity } from "@dfinity/agent";
import { Actor, HttpAgent } from "@dfinity/agent";
import {
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/principal";

import React, { useState } from "react";
import ReactDOM from "react-dom/client";

import { decodeJwt } from "jose";

import { authWithII, extractDelegation } from "./auth";

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

let iiProtocolTestWindow: Window | undefined;

// The identity set by the authentication
let delegationIdentity: DelegationIdentity | undefined = undefined;

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
  return IDL.Service({
    http_request: IDL.Func([HttpRequest], [HttpResponse], ["query"]),
    update_alternative_origins: IDL.Func(
      [IDL.Text, AlternativeOriginsMode],
      [],
      [],
    ),
    whoami: IDL.Func([], [IDL.Principal], ["query"]),
  });
};

const updateDelegationView = ({
  authnMethod,
  identity,
}: {
  authnMethod?: string;
  identity: Identity;
}) => {
  principalEl.innerText = identity.getPrincipal().toText();

  if (authnMethod !== undefined) {
    authnMethodEl.innerText = authnMethod;
  }

  if (identity instanceof DelegationIdentity) {
    delegationEl.innerText = JSON.stringify(
      identity.getDelegation().toJSON(),
      undefined,
      2,
    );

    // cannot use Math.min, as we deal with bigint here
    const nextExpiration = identity
      .getDelegation()
      .delegations.map((d) => d.delegation.expiration)
      .reduce((current, next) => (next < current ? next : current));
    expirationEl.innerText = (
      nextExpiration -
      BigInt(Date.now()) * BigInt(1000_000)
    ).toString();
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

const init = async () => {
  signInBtn.onclick = async () => {
    const maxTimeToLive_ = BigInt(maxTimeToLiveEl.value);
    // The default max TTL setin the @dfinity/auth-client library
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
      });
      delegationIdentity = result.identity;
      updateDelegationView({
        identity: delegationIdentity,
        authnMethod: result.authnMethod,
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
    const httpAgent = new HttpAgent({ host: hostUrlEl.value });
    await httpAgent.fetchRootKey();
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

init();

whoamiBtn.addEventListener("click", async () => {
  const canisterId = Principal.fromText(readCanisterId());
  const agent = new HttpAgent({
    host: hostUrlEl.value,
    identity: delegationIdentity,
  });
  await agent.fetchRootKey();
  const actor = Actor.createActor(idlFactory, {
    agent,
    canisterId,
  });

  whoAmIResponseEl.innerText = "Loading...";

  // Similar to the sample project on dfx new:
  actor
    .whoami()
    .then((principal: any) => {
      whoAmIResponseEl.innerText = principal.toText();
    })
    .catch((err) => {
      console.error("Failed to fetch whoami", err);
    });
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
