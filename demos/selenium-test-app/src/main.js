import { Actor, HttpAgent } from "@dfinity/agent";
import {
  Delegation,
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
} from "@dfinity/identity";
import { AuthClient } from "@dfinity/auth-client";
import { Principal } from "@dfinity/principal";

const signInBtn = document.getElementById("signinBtn");
const signOutBtn = document.getElementById("signoutBtn");
const whoamiBtn = document.getElementById("whoamiBtn");
const openIiWindowBtn = document.getElementById("openIiWindowBtn");
const closeIiWindowBtn = document.getElementById("closeIIWindowBtn");
const invalidDataBtn = document.getElementById("invalidDataBtn");
const incompleteMessageBtn = document.getElementById("incompleteMessageBtn");
const validMessageBtn = document.getElementById("validMessageBtn");
const messagesEl = document.getElementById("messages");
const hostUrlEl = document.getElementById("hostUrl");
const whoAmIResponseEl = document.getElementById("whoamiResponse");
const canisterIdEl = document.getElementById("canisterId");
const principalEl = document.getElementById("principal");
const delegationEl = document.getElementById("delegation");
const expirationEl = document.getElementById("expiration");
const iiUrlEl = document.getElementById("iiUrl");
const maxTimeToLiveEl = document.getElementById("maxTimeToLive");

let authClient;
let iiProtocolTestWindow;
let localIdentity;

const updateView = (identity) => {
  principalEl.innerText = identity.getPrincipal();
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
    expirationEl.innerText =
      nextExpiration - BigInt(Date.now()) * BigInt(1000_000);
  } else {
    delegationEl.innerText = "Current identity is not a DelegationIdentity";
    expirationEl.innerText = "N/A";
  }
};

function addMessageElement(message, received) {
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
      const delegations = event.data.delegations.map((signedDelegation) => {
        return {
          delegation: new Delegation(
            signedDelegation.delegation.pubkey,
            signedDelegation.delegation.expiration,
            signedDelegation.delegation.targets
          ),
          signature: signedDelegation.signature.buffer,
        };
      });
      const delegationChain = DelegationChain.fromDelegations(
        delegations,
        event.data.userPublicKey.buffer
      );
      updateView(
        DelegationIdentity.fromDelegation(localIdentity, delegationChain)
      );
    }
  }
});

const init = async () => {
  authClient = await AuthClient.create();
  updateView(authClient.getIdentity());

  signInBtn.onclick = async () => {
    if (BigInt(maxTimeToLiveEl.value) > BigInt(0)) {
      authClient.login({
        identityProvider: iiUrlEl.value,
        maxTimeToLive: BigInt(maxTimeToLive.value),
        onSuccess: () => updateView(authClient.getIdentity()),
      });
    } else {
      authClient.login({
        identityProvider: iiUrlEl.value,
        onSuccess: () => updateView(authClient.getIdentity()),
      });
    }
  };

  signOutBtn.onclick = async () => {
    await authClient.logout();
    updateView(authClient.getIdentity());
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
    const validMessage = {
      kind: "authorize-client",
      sessionPublicKey: new Uint8Array(localIdentity.getPublicKey().toDer()),
    };
    addMessageElement(validMessage, false);
    iiProtocolTestWindow.postMessage(validMessage, iiUrlEl.value);
  };
};

init();

whoamiBtn.addEventListener("click", async () => {
  const identity = await authClient.getIdentity();

  // We either have an Agent with an anonymous identity (not authenticated),
  // or already authenticated agent, or parsing the redirect from window.location.
  const idlFactory = ({ IDL }) =>
    IDL.Service({
      whoami: IDL.Func([], [IDL.Principal], ["query"]),
    });

  const canisterId = Principal.fromText(canisterIdEl.value);

  const actor = Actor.createActor(idlFactory, {
    agent: new HttpAgent({
      host: hostUrlEl.value,
      identity,
    }),
    canisterId,
  });

  whoAmIResponseEl.innerText = "Loading...";

  // Similar to the sample project on dfx new:
  actor
    .whoami()
    .then((principal) => {
      whoAmIResponseEl.innerText = principal.toText();
    })
    .catch((err) => {
      console.error("Failed to fetch whoami", err);
    });
});
