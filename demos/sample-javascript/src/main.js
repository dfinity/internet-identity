import { Actor, HttpAgent, Principal } from "@dfinity/agent";
import { AuthClient } from "@dfinity/auth-client";

const signInBtn = document.getElementById("signinBtn");
const signOutBtn = document.getElementById("signoutBtn");
const whoamiBtn = document.getElementById("whoamiBtn");
const hostUrlEl = document.getElementById("hostUrl");
const whoAmIResponseEl = document.getElementById("whoamiResponse");
const canisterIdEl = document.getElementById("canisterId");
const principalEl = document.getElementById("principal");
const iiUrlEl = document.getElementById("iiUrl");

let authClient;

const init = async () => {
  authClient = await AuthClient.create();
  principalEl.innerText = await authClient.getIdentity().getPrincipal();

  // Redirect to the identity provider
  signInBtn.onclick = async () => {
    authClient.login({
      identityProvider: iiUrlEl.value,
      onSuccess: async () => {
        principalEl.innerText = await authClient.getIdentity().getPrincipal();
      },
    });
  };

  signOutBtn.onclick = async () => {
    authClient.logout();
    principalEl.innerText = await authClient.getIdentity().getPrincipal();
  };
};

init();

whoamiBtn.addEventListener("click", async () => {
  const identity = await authClient.getIdentity();

  // We either have an Agent with an anonymous identity (not authenticated),
  // or already authenticated agent, or parsing the redirect from window.location.
  const idlFactory = ({ IDL }) =>
    IDL.Service({
      whoami: IDL.Func([], [IDL.Principal], ['query']),
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
  actor.whoami().then((principal) => {
    whoAmIResponseEl.innerText = principal.toText();
  });
});
