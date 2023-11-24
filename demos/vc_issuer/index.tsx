import { Actor, ActorSubclass, HttpAgent } from "@dfinity/agent";
import { idlFactory as vc_issuer_idl } from "./generated/vc_issuer_idl";
import { _SERVICE } from "./generated/vc_issuer_types";

import React, { useState } from "react";
import ReactDOM from "react-dom/client";

export class VcIssuer {
  createActor = async (): Promise<ActorSubclass<_SERVICE>> => {
    const agent = new HttpAgent();

    await agent.fetchRootKey();
    // TODO: fetch root key?

    const canisterId = readCanisterId();
    const actor = Actor.createActor<_SERVICE>(vc_issuer_idl, {
      agent,
      canisterId,
    });
    return actor;
  };

  getConsentMessage = async (): Promise<string> => {
    const actor = await this.createActor();
    const result = await actor.vc_consent_message({
      preferences: { language: "en_US" },
      credential_spec: {
        credential_name: "VerifiedEmployee",
        arguments: [[["employerName", { string: "DFINITY Foundation" }]]],
      },
    });

    if ("err" in result) {
      throw new Error(`err: ${JSON.stringify(result)}`);
    }

    return result.ok.consent_message;
  };
}

/** Reads the canister ID from the <script> tag.
 *
 * The canister injects the canister ID as a `data-canister-id` attribute on the script tag, which we then read to figure out where to make the IC calls.
 */
const readCanisterId = (): string => {
  // The backend uses a known element ID so that we can pick up the value from here
  const setupJs = document.querySelector(
    "[data-canister-id]"
  ) as HTMLElement | null;
  if (!setupJs || setupJs.dataset.canisterId === undefined) {
    throw new Error("canisterId is undefined"); // abort further execution of this script
  }

  return setupJs.dataset.canisterId;
};

const main = async () => {
  console.log("Ok issuer");
  const vcIssuer = new VcIssuer();

  const msg = await vcIssuer.getConsentMessage();
  console.log(msg);
};

const App = () => {
  const [consentMessage, setConsentMessage] = useState<string | undefined>(
    undefined
  );
  const [dsbld, setDsbld] = useState<boolean>(false);

  const getAndShowMessage = async () => {
    setDsbld(true);

    try {
      const vcIssuer = new VcIssuer();
      const msg = await vcIssuer.getConsentMessage();
      setConsentMessage(msg);
    } finally {
      setDsbld(false);
    }
  };

  return (
    <main>
      <div>{consentMessage ? consentMessage : "no consent message"}</div>
      <button disabled={dsbld} onClick={() => getAndShowMessage()}>
        Get consent message
      </button>
    </main>
  );
};
// main();

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
