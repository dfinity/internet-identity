import { Actor, ActorSubclass, HttpAgent } from "@dfinity/agent";
import { AuthClient } from "@dfinity/auth-client";
import { Principal } from "@dfinity/principal";

import { idlFactory as vc_issuer_idl } from "./generated/vc_issuer_idl";
import { _SERVICE } from "./generated/vc_issuer_types";

import React, { useState } from "react";
import ReactDOM from "react-dom/client";

export class VcIssuer {
  createActor = async (): Promise<ActorSubclass<_SERVICE>> => {
    const agent = new HttpAgent();

    await agent.fetchRootKey();

    const canisterId = readCanisterId();
    const actor = Actor.createActor<_SERVICE>(vc_issuer_idl, {
      agent,
      canisterId,
    });
    return actor;
  };

  addEmployee = async ({
    principal,
  }: {
    principal: string;
  }): Promise<string> => {
    const actor = await this.createActor();
    const result = await actor.add_employee(Principal.fromText(principal));
    return result;
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

const App = () => {
  const [principal, setPrincipal] = useState<string | undefined>(undefined);
  const [dsbld, setDsbld] = useState<boolean>(false);

  const withDisabled = async <A, _>(fn: () => Promise<A>): Promise<A> => {
    setDsbld(true);
    try {
      return await fn();
    } finally {
      setDsbld(false);
    }
  };

  const authAndShow = () =>
    withDisabled(async () => {
      const authClient = await AuthClient.create();

      await new Promise<void>((resolve, reject) => {
        authClient.login({
          identityProvider: "http://localhost:5173",
          onSuccess: () => resolve(),
          onError: reject,
        });
      });

      setPrincipal(authClient.getIdentity().getPrincipal().toText());
    });

  const addEmployee = (principal: string) =>
    withDisabled(async () => {
      const vcIssuer = new VcIssuer();
      const res = await vcIssuer.addEmployee({ principal });
      console.log("Canister says: ", res);
    });

  return (
    <main>
      <div>{principal ? principal : "not authed"}</div>
      <button disabled={dsbld} onClick={() => authAndShow()}>
        Authenticate
      </button>
      {principal ? (
        <button onClick={() => addEmployee(principal)}>Add employee</button>
      ) : (
        <button disabled={!principal}>Add employee</button>
      )}
    </main>
  );
};

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
