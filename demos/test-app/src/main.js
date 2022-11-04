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
const webAuthnRegisterBtn = document.getElementById("webAuthnRegisterBtn");
const webAuthnSignBtn = document.getElementById("webAuthnSignBtn");
const userEl = document.getElementById("user");
const whoamiBtn = document.getElementById("whoamiBtn");
const updateAlternativeOriginsBtn = document.getElementById(
  "updateNewAlternativeOrigins"
);
const openIiWindowBtn = document.getElementById("openIiWindowBtn");
const closeIiWindowBtn = document.getElementById("closeIIWindowBtn");
const invalidDataBtn = document.getElementById("invalidDataBtn");
const incompleteMessageBtn = document.getElementById("incompleteMessageBtn");
const validMessageBtn = document.getElementById("validMessageBtn");
const customMessageEl = document.getElementById("customMessage");
const customMessageBtn = document.getElementById("customMessageBtn");
const messagesEl = document.getElementById("messages");
const hostUrlEl = document.getElementById("hostUrl");
const whoAmIResponseEl = document.getElementById("whoamiResponse");
const alternativeOriginsEl = document.getElementById("alternativeOrigins");
const newAlternativeOriginsEl = document.getElementById(
  "newAlternativeOrigins"
);
const canisterIdEl = document.getElementById("canisterId");
const principalEl = document.getElementById("principal");
const delegationEl = document.getElementById("delegation");
const expirationEl = document.getElementById("expiration");
const iiUrlEl = document.getElementById("iiUrl");
const maxTimeToLiveEl = document.getElementById("maxTimeToLive");
const derivationOriginEl = document.getElementById("derivationOrigin");

let authClient;
let iiProtocolTestWindow;
let localIdentity;

const idlFactory = ({ IDL }) => {
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

const updateDelegationView = (identity) => {
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

const updateAlternativeOriginsView = async () => {
  const response = await fetch("/.well-known/ii-alternative-origins");
  alternativeOriginsEl.innerText = await response.text();
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
      updateDelegationView(
        DelegationIdentity.fromDelegation(localIdentity, delegationChain)
      );
    }
  }
});

const init = async () => {
  authClient = await AuthClient.create();
  updateDelegationView(authClient.getIdentity());
  await updateAlternativeOriginsView();
  canisterIdEl.value = canisterId;
  signInBtn.onclick = async () => {
    let derivationOrigin =
      derivationOriginEl.value !== "" ? derivationOriginEl.value : undefined;
    if (BigInt(maxTimeToLiveEl.value) > BigInt(0)) {
      authClient.login({
        identityProvider: iiUrlEl.value,
        maxTimeToLive: BigInt(maxTimeToLive.value),
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
    const canisterId = Principal.fromText(canisterIdEl.value);
    const httpAgent = new HttpAgent({ host: hostUrlEl.value });
    await httpAgent.fetchRootKey();
    const actor = Actor.createActor(idlFactory, {
      agent: httpAgent,
      canisterId,
    });
    const modeSelection = document.querySelector(
      'input[name="alternativeOriginsMode"]:checked'
    ).value;
    let mode = { CertifiedContent: null };
    if (modeSelection === "uncertified") {
      mode = { UncertifiedContent: null };
    } else if (modeSelection === "redirect") {
      let location = document.getElementById("redirectLocation").value;
      mode = { Redirect: { location: location } };
    }
    await actor.update_alternative_origins(newAlternativeOriginsEl.value, mode);
    await updateAlternativeOriginsView();
  };

  let challenge = new Uint8Array(100);
  window.crypto.getRandomValues(challenge.slice(0, 100));
  console.log("challenge: " + JSON.stringify(challenge));

  let user_id = new Uint8Array(32);
  for (let i = 0; i < 32; i++) {
    user_id.set([userEl.value * i], i);
  }
  console.log("user id: " + JSON.stringify(user_id));
  let credential = null;

  webAuthnRegisterBtn.onclick = async () => {
    console.log("register");
    const options = {
      publicKey: {
        authenticatorSelection: {
          requireResidentKey: true, // not supported on FF
          attestation: "indirect",
        },
        challenge: challenge,
        rp: {
          displayName: "Test Application",
          name: "Test Application",
          id: window.location.hostname,
        },
        user: {
          displayName: "Test User " + userEl.value,
          name: "Test User " + userEl.value,
          id: user_id,
        },
        pubKeyCredParams: [
          {
            type: "public-key",
            // alg: PubKeyCoseAlgo.ECDSA_WITH_SHA256
            alg: -7,
          },
          {
            type: "public-key",
            // alg: PubKeyCoseAlgo.RSA_WITH_SHA256
            alg: -257,
          },
        ],
      },
    };
    console.log("options: " + JSON.stringify(options));
    credential = await navigator.credentials.create(options);
    const clientDataStr = arrayBufferToStr(credential.response.clientDataJSON);
    console.log("credential: " + credential.id);
    console.log("client data: " + clientDataStr);
    console.log(
      "attestation object: " + buf2hex(credential.response.attestationObject)
    );
  };

  webAuthnSignBtn.onclick = async () => {
    console.log("sign");
    // this prompts an authentication selector from chrome, fails in FF
    // const options = {
    //   publicKey: {
    //     challenge: challenge,
    //     rpId: "localhost",
    //   },
    // };
    const options = {
      publicKey: {
        challenge: challenge,
        //allowCredentials: [{ id: credential.rawId, type: "public-key" }],
      },
    };
    let result = await navigator.credentials.get(options);
    console.log("credential: " + result.id);

    console.log(result);
  };
};

function arrayBufferToStr(buf) {
  return String.fromCharCode.apply(null, new Uint8Array(buf));
}

function buf2hex(buffer) {
  // buffer is an ArrayBuffer
  return [...new Uint8Array(buffer)]
    .map((x) => x.toString(16).padStart(2, "0"))
    .join("");
}

init();

whoamiBtn.addEventListener("click", async () => {
  const identity = await authClient.getIdentity();
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
