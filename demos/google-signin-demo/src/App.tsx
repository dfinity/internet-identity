import React from "react";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import { authenticate, JsonToCertifiedAttributesCodec, verify } from "./utils";

// Internet Identity
const iiCanisterId = Principal.fromText("jqajs-xiaaa-aaaad-aab5q-cai");
const iiURL = `https://${iiCanisterId}.ic0.app`; // `https://id.ai` on prod

// OpenID issuer and attributes
const googleIssuer = "https://accounts.google.com";
const googleNameAttribute = `openid:${googleIssuer}:name`;
const googleEmailAttribute = `openid:${googleIssuer}:email`;

// Session: created in backend and public key sent to frontend
let sessionIdentity = ECDSAKeyIdentity.generate();

const App: React.FC = () => {
  const [name, setName] = React.useState("");
  const [email, setEmail] = React.useState("");
  const [principal, setPrincipal] = React.useState("");
  const [isSignedIn, setIsSignedIn] = React.useState(false);

  // Client side
  const handleSignIn = async () => {
    const { delegationChain, certifiedAttributes } = await authenticate({
      identityProvider: iiURL,
      sessionPublicKey: (await sessionIdentity).getPublicKey().toDer(),
      // Below properties are optional, omit to authenticate without OpenID
      directOpenIdAuth: googleIssuer,
      attributes: [googleNameAttribute, googleEmailAttribute],
    });

    // Send JSON payload to server
    await handleAuthRequest(
      JSON.stringify({
        delegationChain: delegationChain.toJSON(),
        certifiedAttributes:
          JsonToCertifiedAttributesCodec.encode(certifiedAttributes),
      }),
    );
  };

  // Server side
  const handleAuthRequest = async (payload: string) => {
    // Parse payload
    const data = JSON.parse(payload);
    const delegationChain = DelegationChain.fromJSON(data.delegationChain);
    const certifiedAttributes = JsonToCertifiedAttributesCodec.parse(
      data.certifiedAttributes,
    );

    // Verify all signatures
    const valid = await verify({
      sessionPublicKey: (await sessionIdentity).getPublicKey().toDer(),
      canisterId: iiCanisterId,
      delegationChain,
      certifiedAttributes,
    });
    if (!valid) {
      throw new Error("Invalid authentication data");
    }

    // Verified principal and attributes
    const principal = Principal.selfAuthenticating(delegationChain.publicKey);
    const name = certifiedAttributes[googleNameAttribute].value;
    const email = certifiedAttributes[googleEmailAttribute].value;

    // For this demo: Update interface with principal and email attribute
    setPrincipal(principal.toText());
    setName(new TextDecoder().decode(name));
    setEmail(new TextDecoder().decode(email));
    setIsSignedIn(true);
  };

  // Client side
  const handleSignOut = () => {
    setPrincipal("");
    setName("");
    setEmail("");
    setIsSignedIn(false);

    // Server side (session should be reset)
    sessionIdentity = ECDSAKeyIdentity.generate();
  };

  return (
    <main>
      <div className="card">
        <h1>Welcome Back</h1>
        <p
          style={{
            color: "var(--text-muted)",
            margin: 0,
            fontSize: "14px",
            textAlign: "center",
          }}
        >
          {isSignedIn
            ? "You are signed in"
            : "Sign in to your account to continue"}
        </p>

        {isSignedIn ? (
          <button
            onClick={handleSignOut}
            className="btn-google"
            style={{ backgroundColor: "#fee", color: "#b91c1c" }}
          >
            <svg
              width="18"
              height="18"
              viewBox="0 0 24 24"
              fill="none"
              stroke="currentColor"
              strokeWidth="2"
              strokeLinecap="round"
              strokeLinejoin="round"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4"></path>
              <polyline points="16 17 21 12 16 7"></polyline>
              <line x1="21" y1="12" x2="9" y2="12"></line>
            </svg>
            Sign out
          </button>
        ) : (
          <button onClick={handleSignIn} className="btn-google">
            <svg
              width="18"
              height="18"
              viewBox="0 0 18 18"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path
                d="M17.64 9.2c0-.637-.057-1.251-.164-1.84H9v3.481h4.844c-.209 1.125-.843 2.078-1.796 2.717v2.258h2.908c1.702-1.567 2.684-3.874 2.684-6.615z"
                fill="#4285F4"
              />
              <path
                d="M9 18c2.43 0 4.467-.806 5.956-2.184L12.048 13.56c-.829.556-1.892.883-3.048.883-2.344 0-4.328-1.584-5.036-3.711H.957v2.332A8.997 8.997 0 009 18z"
                fill="#34A853"
              />
              <path
                d="M3.964 10.73A5.41 5.41 0 013.682 9c0-.603.103-1.188.282-1.731V4.937H.957A8.992 8.992 0 000 9c0 1.451.345 2.827.957 4.063l3.007-2.332z"
                fill="#FBBC05"
              />
              <path
                d="M9 3.58c1.321 0 2.508.454 3.44 1.345l2.582-2.58C13.463.891 11.426 0 9 0 5.483 0 2.443 2.017.957 4.937l3.007 2.332C4.672 5.163 6.656 3.58 9 3.58z"
                fill="#EA4335"
              />
            </svg>
            Sign in with Google
          </button>
        )}

        <div
          style={{
            width: "100%",
            height: "1px",
            backgroundColor: "#e5e7eb",
            margin: "8px 0",
          }}
        ></div>

        <div className="input-group">
          <label htmlFor="principal">Principal</label>
          <input
            id="principal"
            type="text"
            placeholder="Principal ID"
            value={principal}
            readOnly
          />
        </div>

        <div className="input-group">
          <label htmlFor="email">Name</label>
          <input
            id="name"
            type="text"
            placeholder="John Doe"
            value={name}
            readOnly
          />
        </div>

        <div className="input-group">
          <label htmlFor="email">Email Address</label>
          <input
            id="email"
            type="email"
            placeholder="your@email.com"
            value={email}
            readOnly
          />
        </div>
      </div>
    </main>
  );
};

export default App;
