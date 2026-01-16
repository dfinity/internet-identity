import React from "react";
import { Signer } from "@slide-computer/signer";
import { PostMessageTransport } from "@slide-computer/signer-web";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";

const transport = new PostMessageTransport({
    url: "https://id.ai/authorize?openid=google"
});

const signer = new Signer({ transport });

const App: React.FC = () => {
    const [email, setEmail] = React.useState("");

    const handleSignIn = async () => {
        try {
            const publicKey = (await ECDSAKeyIdentity.generate()).getPublicKey().toDer();

            const result_promise = signer.delegation({
                publicKey
            });

            const attribute_promise = signer.sendRequest({
                jsonrpc: "2.0",
                method: "ii_attributes",
                id: 123,
                params: {
                    attributes: [
                        "email"
                    ]
                }
            });

            const [result, attributesResponse] = await Promise.all([result_promise, attribute_promise]);

            console.log("Sign in result", result);
            console.log("Attribute result", JSON.stringify(attributesResponse, null, 2));

            // Extract email from attributes response
            // The response might be the full JSON-RPC object or just the result
            const attributes = (attributesResponse as any)?.result || attributesResponse;
            if (attributes && attributes.email) {
                setEmail(attributes.email);
            }
        } catch (error) {
            console.error("Sign in failed", error);
        }
    };

    return (
        <main>
            <div className="card">
                <h1>Welcome Back</h1>
                <p style={{ color: 'var(--text-muted)', margin: 0, fontSize: '14px', textAlign: 'center' }}>
                    Sign in to your account to continue
                </p>

                <button
                    onClick={handleSignIn}
                    className="btn-google"
                >
                    <svg width="18" height="18" viewBox="0 0 18 18" xmlns="http://www.w3.org/2000/svg">
                        <path d="M17.64 9.2c0-.637-.057-1.251-.164-1.84H9v3.481h4.844c-.209 1.125-.843 2.078-1.796 2.717v2.258h2.908c1.702-1.567 2.684-3.874 2.684-6.615z" fill="#4285F4" />
                        <path d="M9 18c2.43 0 4.467-.806 5.956-2.184L12.048 13.56c-.829.556-1.892.883-3.048.883-2.344 0-4.328-1.584-5.036-3.711H.957v2.332A8.997 8.997 0 009 18z" fill="#34A853" />
                        <path d="M3.964 10.73A5.41 5.41 0 013.682 9c0-.603.103-1.188.282-1.731V4.937H.957A8.992 8.992 0 000 9c0 1.451.345 2.827.957 4.063l3.007-2.332z" fill="#FBBC05" />
                        <path d="M9 3.58c1.321 0 2.508.454 3.44 1.345l2.582-2.58C13.463.891 11.426 0 9 0 5.483 0 2.443 2.017.957 4.937l3.007 2.332C4.672 5.163 6.656 3.58 9 3.58z" fill="#EA4335" />
                    </svg>
                    Sign in with Google
                </button>

                <div style={{ width: '100%', height: '1px', backgroundColor: '#e5e7eb', margin: '8px 0' }}></div>

                <div className="input-group">
                    <label htmlFor="email">Email Address</label>
                    <input
                        id="email"
                        type="email"
                        placeholder="your@email.com"
                        value={email}
                        onChange={(e) => setEmail(e.target.value)}
                    />
                </div>
            </div>
        </main>
    );
};

export default App;

