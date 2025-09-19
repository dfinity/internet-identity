import React, { useState, useEffect } from "react";
import {
  DelegationIdentity,
  DelegationChain,
  ECDSAKeyIdentity,
} from "@dfinity/identity";
import { AuthClient } from "@dfinity/auth-client";
import { HttpAgent } from "@dfinity/agent";
import { createActor } from "declarations/internet_identity_app_backend";
import { canisterId } from "declarations/internet_identity_app_backend/index.js";

const identityProvider = "http://localhost:5173/";

// Reusable button component
const Button = ({ onClick, children }) => (
  <button onClick={onClick}>{children}</button>
);

const App = () => {
  const [state, setState] = useState({
    actor: undefined,
    authClient: undefined,
    isAuthenticated: false,
    principal: 'Click "Whoami" to see your principal ID',
  });

  // Initialize auth client
  useEffect(() => {
    updateActor();
  }, []);

  const restorePublicKeyIdentity = async (derBase64) => {
    const der = Uint8Array.from(atob(derBase64), (c) => c.charCodeAt(0));

    const publicKey = await crypto.subtle.importKey(
      "spki",
      der.buffer,
      { name: "ECDSA", namedCurve: "P-256" },
      true,
      ["verify"]
    );

    return ECDSAKeyIdentity.fromKeyPair({ publicKey });
  };

  const rebuildDelegationIdentity = async (delegationJson, userKeyBase64) => {
    const inner = await restorePublicKeyIdentity(userKeyBase64);
    const chain = DelegationChain.fromJSON(JSON.parse(delegationJson));
    return DelegationIdentity.fromDelegation(inner, chain);
  };

  const updateActor = async () => {
    const storedDelegation = localStorage.getItem("delegation_identity");
    const appKeypairRaw = localStorage.getItem("openid_app_keypair");

    console.log(storedDelegation);
    console.log(appKeypairRaw);
    const identity =
      storedDelegation && userKey
        ? await rebuildDelegationIdentity(storedDelegation, userKey)
        : undefined;

    console.log("rebuiltIdentity principal:", identity.getPrincipal().toText());
    console.log(
      "rebuiltIdentity delegation:",
      JSON.stringify(identity.getDelegation().toJSON())
    );

    console.log(identity);
    const agent = new HttpAgent({ identity });
    const actor = createActor(canisterId, {
      agent,
    });

    setState((prev) => ({
      ...prev,
      actor,
      isAuthenticated: !!identity,
    }));
  };

  const login = async () => {
    const appIdentity = await window.crypto.subtle.generateKey(
      { name: "ECDSA", namedCurve: "P-256" },
      true,
      ["sign", "verify"]
    );
    const exported = {
      privateKey: await window.crypto.subtle.exportKey(
        "jwk",
        appIdentity.privateKey
      ),
      publicKey: await window.crypto.subtle.exportKey(
        "jwk",
        appIdentity.publicKey
      ),
    };
    localStorage.setItem("openid_app_keypair", JSON.stringify(exported));

    const redirectUri = window.location.origin + "/";
    const provider = "google";
    const maxTimeToLive = 15 * 60 * 1000;
    const derivationOrigin = window.location.origin;

    const url = new URL("http://localhost:5173/auth-redirect");
    url.searchParams.set("redirectUri", redirectUri);
    url.searchParams.set("derivationOrigin", derivationOrigin);
    url.searchParams.set("appKeypair", JSON.stringify(exported));
    url.searchParams.set("provider", provider);
    url.searchParams.set("maxTimeToLive", maxTimeToLive.toString());

    // ✅ open in new tab
    window.open(url.toString(), "_blank", "noopener,noreferrer");
  };

  const logout = async () => {
    await state.authClient.logout();
    updateActor();
  };

  const whoami = async () => {
    setState((prev) => ({
      ...prev,
      principal: "Loading...",
    }));

    const result = await state.actor.whoami();
    console.log(result);
    const principal = result.toString();
    setState((prev) => ({
      ...prev,
      principal,
    }));
  };

  useEffect(() => {
    const hash = new URLSearchParams(window.location.hash.slice(1));
    console.log(hash);
    const delegationJson = hash.get("identity");
    const userKey = hash.get("userKey");
    console.log(delegationJson);
    console.log(userKey);

    if (delegationJson && userKey) {
      // Store delegation for later
      localStorage.setItem("delegation_identity", delegationJson);

      // Clear hash and reload app
      updateActor();
    } else {
      updateActor();
    }
  }, []);

  return (
    <div>
      <h1>Who Am I?</h1>
      <div id="info-box" className="info-box">
        <div className="info-content">
          <p>
            <i className="fas fa-info-circle"></i> A <strong>principal</strong>{" "}
            is a unique identifier in the Internet Computer ecosystem.
          </p>
          <p>
            It represents an entity (user, canister smart contract, or other)
            and is used for identification and authorization purposes.
          </p>
          <p>
            In this example, click "Whoami" to find out the principal ID with
            which you're interacting with the backend. If you're not signed in,
            you will see that you're using the so-called anonymous principal,
            "2vxsx-fae".
          </p>
          <p>
            After you've logged in with Internet Identity, you'll see a longer
            principal, which is unique to your identity and the dapp you're
            using.
          </p>
        </div>
      </div>

      {!state.isAuthenticated ? (
        <Button onClick={login}>Login with Internet Identity</Button>
      ) : (
        <Button onClick={logout}>Logout</Button>
      )}

      <Button onClick={whoami}>Whoami</Button>

      {state.principal && (
        <div>
          <h2>Your principal ID is:</h2>
          <h4>{state.principal}</h4>
        </div>
      )}
    </div>
  );
};

export default App;
