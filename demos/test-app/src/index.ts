import type { Identity, SignIdentity } from "@dfinity/agent";
import { Actor, HttpAgent } from "@dfinity/agent";
import { AuthClient } from "@dfinity/auth-client";
import {
  Delegation,
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/principal";

import "./main.css";

const signInBtn = document.getElementById("signinBtn") as HTMLButtonElement;
const signOutBtn = document.getElementById("signoutBtn") as HTMLButtonElement;
const whoamiBtn = document.getElementById("whoamiBtn") as HTMLButtonElement;
const updateAlternativeOriginsBtn = document.getElementById(
  "updateNewAlternativeOrigins"
) as HTMLButtonElement;
const openIiWindowBtn = document.getElementById(
  "openIiWindowBtn"
) as HTMLButtonElement;
const closeIiWindowBtn = document.getElementById(
  "closeIIWindowBtn"
) as HTMLButtonElement;
const invalidDataBtn = document.getElementById(
  "invalidDataBtn"
) as HTMLButtonElement;
const incompleteMessageBtn = document.getElementById(
  "incompleteMessageBtn"
) as HTMLButtonElement;
const validMessageBtn = document.getElementById(
  "validMessageBtn"
) as HTMLButtonElement;
const customMessageEl = document.getElementById(
  "customMessage"
) as HTMLInputElement;
const customMessageBtn = document.getElementById(
  "customMessageBtn"
) as HTMLButtonElement;
const messagesEl = document.getElementById("messages") as HTMLElement;
const hostUrlEl = document.getElementById("hostUrl") as HTMLInputElement;
const whoAmIResponseEl = document.getElementById("whoamiResponse");
const alternativeOriginsEl = document.getElementById(
  "alternativeOrigins"
) as HTMLDivElement;
const newAlternativeOriginsEl = document.getElementById(
  "newAlternativeOrigins"
) as HTMLInputElement;
const principalEl = document.getElementById("principal") as HTMLDivElement;
const delegationEl = document.getElementById("delegation") as HTMLPreElement;
const expirationEl = document.getElementById("expiration") as HTMLDivElement;
const iiUrlEl = document.getElementById("iiUrl") as HTMLInputElement;
const maxTimeToLiveEl = document.getElementById(
  "maxTimeToLive"
) as HTMLInputElement;
const derivationOriginEl = document.getElementById(
  "derivationOrigin"
) as HTMLInputElement;

let authClient: AuthClient;
let iiProtocolTestWindow: Window | undefined;
let localIdentity: SignIdentity;

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
      []
    ),
    whoami: IDL.Func([], [IDL.Principal], ["query"]),
  });
};

const updateDelegationView = (identity: Identity) => {
  principalEl.innerText = identity.getPrincipal().toText();
  if (identity instanceof DelegationIdentity) {
    delegationEl.innerText = JSON.stringify(
      identity.getDelegation().toJSON(),
      undefined,
      2
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

function addMessageElement(message: unknown, received: boolean) {
  const messageContainer = document.createElement("div");
  messageContainer.classList.add("postMessage");
  const messageTitle = document.createElement("div");
  messageTitle.classList.add("postMessage-title");
  const messageContent = document.createElement("div");
  messageContent.innerText = JSON.stringify(message, (_, v) =>
    typeof v === "bigint" ? v.toString() : v
  );
  if (received) {
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
  if (event.source === iiProtocolTestWindow) {
    addMessageElement(event.data, true);
    if (event?.data?.kind === "authorize-client-success") {
      const delegations = event.data.delegations.map(
        (signedDelegation: any) => {
          return {
            delegation: new Delegation(
              signedDelegation.delegation.pubkey,
              signedDelegation.delegation.expiration,
              signedDelegation.delegation.targets
            ),
            signature: signedDelegation.signature.buffer,
          };
        }
      );
      const delegationChain = DelegationChain.fromDelegations(
        delegations,
        event.data.userPublicKey.buffer
      );
      updateDelegationView(
        DelegationIdentity.fromDelegation(localIdentity, delegationChain)
      );
    }
  }
});

const readCanisterId = (): string => {
  const canIdEl = document.querySelector("[data-canister-id]") as HTMLElement;
  return canIdEl.dataset.canisterId!;
};

const init = async () => {
  authClient = await AuthClient.create();
  updateDelegationView(authClient.getIdentity());
  await updateAlternativeOriginsView();
  signInBtn.onclick = async () => {
    let derivationOrigin =
      derivationOriginEl.value !== "" ? derivationOriginEl.value : undefined;
    if (BigInt(maxTimeToLiveEl.value) > BigInt(0)) {
      authClient.login({
        identityProvider: iiUrlEl.value,
        maxTimeToLive: BigInt(maxTimeToLiveEl.value),
        derivationOrigin,
        onSuccess: () => updateDelegationView(authClient.getIdentity()),
      });
    } else {
      authClient.login({
        identityProvider: iiUrlEl.value,
        derivationOrigin,
        onSuccess: () => updateDelegationView(authClient.getIdentity()),
      });
    }
  };

  signOutBtn.onclick = async () => {
    await authClient.logout();
    updateDelegationView(authClient.getIdentity());
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

  invalidDataBtn!.onclick = () => {
    if (!iiProtocolTestWindow) {
      alert("Open II tab first");
      return;
    }
    const invalidData = "some invalid data";
    addMessageElement(invalidData, false);
    iiProtocolTestWindow.postMessage(invalidData, iiUrlEl.value);
  };

  incompleteMessageBtn.onclick = () => {
    if (!iiProtocolTestWindow) {
      alert("Open II tab first");
      return;
    }
    const incompleteMessage = { kind: "authorize-client" };
    addMessageElement(incompleteMessage, false);
    iiProtocolTestWindow.postMessage(incompleteMessage, iiUrlEl.value);
  };

  validMessageBtn.onclick = () => {
    if (!iiProtocolTestWindow) {
      alert("Open II tab first");
      return;
    }
    localIdentity = Ed25519KeyIdentity.generate();
    let derivationOrigin =
      derivationOriginEl.value !== "" ? derivationOriginEl.value : undefined;
    let maxTimeToLive =
      BigInt(maxTimeToLiveEl.value) > BigInt(0)
        ? BigInt(maxTimeToLiveEl.value)
        : undefined;
    const validMessage = {
      kind: "authorize-client",
      sessionPublicKey: new Uint8Array(localIdentity.getPublicKey().toDer()),
      derivationOrigin,
      maxTimeToLive,
    };
    addMessageElement(validMessage, false);
    iiProtocolTestWindow.postMessage(validMessage, iiUrlEl.value);
  };

  customMessageBtn.onclick = () => {
    if (!iiProtocolTestWindow) {
      alert("Open II tab first");
      return;
    }
    let message = JSON.parse(customMessageEl.value);
    addMessageElement(message, false);
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
        'input[name="alternativeOriginsMode"]:checked'
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
};

init();

whoamiBtn!.addEventListener("click", async () => {
  const identity = await authClient.getIdentity();
  const canisterId = Principal.fromText(readCanisterId());
  const actor = Actor.createActor(idlFactory, {
    agent: new HttpAgent({
      host: (hostUrlEl as HTMLInputElement).value,
      identity,
    }),
    canisterId,
  });

  whoAmIResponseEl!.innerText = "Loading...";

  // Similar to the sample project on dfx new:
  actor
    .whoami()
    .then((principal: any) => {
      whoAmIResponseEl!.innerText = principal.toText();
    })
    .catch((err) => {
      console.error("Failed to fetch whoami", err);
    });
});
