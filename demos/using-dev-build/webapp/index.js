/* A simple webapp that authenticates the user with Internet Identity and that
 * then calls the whoami canister to check the user's principal.
 */

import { Actor, HttpAgent } from "@dfinity/agent";
import { AuthClient } from "@dfinity/auth-client";

const webapp_id = process.env.WHOAMI_CANISTER_ID;

// The interface of the whoami canister
const webapp_idl = ({ IDL }) => {
  return IDL.Service({ whoami: IDL.Func([], [IDL.Principal], ["query"]) });
};
export const init = ({ IDL }) => {
  return [];
};

// Autofills the <input> for the II Url to point to the correct canister.
document.body.onload = () => {
  const testnetII =
    process.env.DFX_NETWORK == "local"
      ? `http://localhost:8000/?canisterId=${process.env.II_CANISTER_ID}`
      : process.env.DFX_NETWORK == "ic"
      ? `https://${process.env.II_CANISTER_ID}.ic0.app`
      : `https://${process.env.II_CANISTER_ID}.dfinity.network`;
  document.getElementById("iiUrl").value = testnetII;
};

document.getElementById("loginBtn").addEventListener("click", async () => {
  // When the user clicks, we let agent-js do its thing and redirect open an II
  // tab for authentication
  const iiUrl = document.getElementById("iiUrl").value;
  const authClient = await AuthClient.create();

  // This API is just weird
  await new Promise((resolve, reject) => {
    authClient.login({
      identityProvider: iiUrl,
      onSuccess: resolve,
      onError: reject,
    });
  });

  // At this point, we're authenticated with II and we can query the whoami canister
  const identity = authClient.getIdentity();
  const agent = new HttpAgent({ identity });
  const webapp = Actor.createActor(webapp_idl, {
    agent,
    canisterId: webapp_id,
  });
  const whoAmI = await webapp.whoami();

  document.getElementById("loginStatus").innerText = whoAmI.toText();
});
