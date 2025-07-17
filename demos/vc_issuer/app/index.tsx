import { AuthClient } from "@dfinity/auth-client";

import React, { useState } from "react";
import ReactDOM from "react-dom/client";

import { VcIssuer } from "./issuer";

import "./main.css";

/** Reads the canister ID from the <script> tag.
 *
 * The canister injects the canister ID as a `data-canister-id` attribute on the script tag, which we then read to figure out where to make the IC calls.
 */
const readCanisterId = (): string => {
  // The backend uses a known element ID so that we can pick up the value from here
  const setupJs = document.querySelector(
    "[data-canister-id]",
  ) as HTMLElement | null;
  if (!setupJs || setupJs.dataset.canisterId === undefined) {
    throw new Error("canisterId is undefined"); // abort further execution of this script
  }

  return setupJs.dataset.canisterId;
};

/** The issuer management app */
const App = () => {
  // A global "disabled" flag that gets set during async operations
  // to prevent the user to interact with the app until the operation
  // is completed.
  const [dsbld, setDsbld] = useState<boolean>(false);

  // Run an async op while disabling buttons/interaction
  const withDisabled = async <A, _>(fn: () => Promise<A>): Promise<A> => {
    setDsbld(true);
    try {
      return await fn();
    } finally {
      setDsbld(false);
    }
  };

  // The derivation origin to use for authentication
  const [derivationOrigin, setDerivationOrigin] = useState<string | undefined>(
    undefined,
  );

  // The principal, set during auth
  const [principal, setPrincipal] = useState<string | undefined>(undefined);

  // The canister logs
  const [canisterLogs, setCanisterLogs] = useState<string[]>([]);

  // The URL used for connecting to the IDP
  const [iiUrl, setIiUrl] = useState<string>("http://localhost:5173");

  // Perform authentication with IDP and show principal
  const authAndShow = () =>
    withDisabled(async () => {
      const authClient = await AuthClient.create();

      await new Promise<void>((resolve, reject) => {
        authClient.login({
          identityProvider: iiUrl,
          derivationOrigin,
          onSuccess: () => resolve(),
          onError: reject,
        });
      });

      setPrincipal(authClient.getIdentity().getPrincipal().toText());
    });

  // Add the principal as employee
  const addEmployee = (principal: string) =>
    withDisabled(async () => {
      const canisterId = readCanisterId();
      const vcIssuer = new VcIssuer(canisterId);
      const res: string = await vcIssuer.addEmployee({ principal });
      setCanisterLogs([...canisterLogs, res]);
    });
  const addAdult = (principal: string) =>
    withDisabled(async () => {
      const canisterId = readCanisterId();
      const vcIssuer = new VcIssuer(canisterId);
      const res: string = await vcIssuer.addAdult({ principal });
      setCanisterLogs([...canisterLogs, res]);
    });

  return (
    <main data-page="add-employee">
      <h1>Add Employee</h1>
      <section>
        <label>
          Principal:
          {principal ? (
            <output data-role="principal">{principal}</output>
          ) : (
            <output data-role="principal" data-unset>
              not authenticated
            </output>
          )}
        </label>
      </section>
      <section>
        <label>
          Canister Replies:
          {canisterLogs.length === 0 ? (
            <output data-unset data-role="canister-logs"></output>
          ) : (
            <output data-role="canister-logs">{canisterLogs.join("\n")}</output>
          )}
        </label>
      </section>
      <section>
        <label>
          Internet Identity URL:
          <input
            data-role="ii-url"
            type="text"
            value={iiUrl}
            onChange={(evt) => setIiUrl(evt.target.value)}
          />
        </label>
      </section>
      <section>
        <label>
          Custom principal:
          <input
            data-role="custom-principal"
            type="text"
            onChange={(evt) => setPrincipal(evt.target.value)}
          />
        </label>
      </section>
      <section>
        <label>
          Derivation origin (for authentication):
          <input
            data-role="derivation-origin"
            type="text"
            onChange={(evt) => setDerivationOrigin(evt.target.value)}
          />
        </label>
      </section>
      <section>
        <button
          data-action="authenticate"
          disabled={dsbld}
          onClick={() => authAndShow()}
        >
          Authenticate
        </button>
        {principal ? (
          <button
            data-action="add-employee"
            disabled={dsbld}
            onClick={() => addEmployee(principal)}
          >
            Add employee
          </button>
        ) : (
          <button data-action="add-employee" disabled={dsbld || !principal}>
            Add employee
          </button>
        )}
        {principal ? (
          <button
            data-action="add-adult"
            disabled={dsbld}
            onClick={() => addAdult(principal)}
          >
            Add Adult
          </button>
        ) : (
          <button data-action="add-adult" disabled={dsbld || !principal}>
            Add Adult
          </button>
        )}
      </section>
    </main>
  );
};

ReactDOM.createRoot(document.getElementById("root")!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
);
