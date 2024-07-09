// @vitest-environment node
// We need to run this test in a node environment because of some incompatibility of the Uint8Array in Node and JS DOM
// Reference: https://github.com/vitest-dev/vitest/issues/4043#issuecomment-1713026327
import { idlFactory as internet_identity_idl } from "$generated/internet_identity_idl";
import { _SERVICE } from "$generated/internet_identity_types";
import { Actor, AnonymousIdentity, HttpAgent } from "@dfinity/agent";
import { ECDSAKeyIdentity } from "@dfinity/identity";
import { subtle } from "crypto";
import { DEVICE_NAME1, II_CANISTER_ID, II_URL, REPLICA_URL } from "./constants";
import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  getWebAuthnCredentials,
  runInBrowser,
} from "./util";
import { MainView, WelcomeView } from "./views";

// Function to convert ArrayBuffer to base64
function uint8ArrayToBase64Url(uint8Array: Uint8Array) {
  // Convert Uint8Array to Buffer
  const buffer = Buffer.from(uint8Array);
  // Convert Buffer to base64
  const base64 = buffer.toString("base64");
  // Convert base64 to base64url by replacing characters and removing padding
  const base64url = base64
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");
  return base64url;
}

// function base64ToArrayBuffer(base64: string): ArrayBuffer {
//   const binaryString = atob(base64);
//   const len = binaryString.length;
//   const bytes = new Uint8Array(len);
//   for (let i = 0; i < len; i++) {
//     bytes[i] = binaryString.charCodeAt(i);
//   }
//   return bytes.buffer;
// }

function base64ToArrayBuffer(base64: string): ArrayBuffer {
  const buffer = Buffer.from(base64, "base64");
  const arrayBuffer = new ArrayBuffer(buffer.length);
  const uint8Array = new Uint8Array(arrayBuffer);

  for (let i = 0; i < buffer.length; i++) {
    uint8Array[i] = buffer[i];
  }

  return arrayBuffer;
}

function base64ToBase64Url(base64: string): string {
  return base64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}

// Function to convert JWK private key component to base64url
function convertECDSAPrivateKeyToBase64Url(jwk: JsonWebKey): string {
  // eslint-disable-next-line
  if (jwk.hasOwnProperty("d")) {
    return base64ToBase64Url(jwk["d"] ?? "");
  }

  throw new Error("Invalid JWK");
}

test("User sees the recovery phrase warning page if one week has passed since last time", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authId = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);

    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    // await new Promise((r) => setTimeout(r, 20_000));

    // const authId = randomUUID();
    const initialCredentials = await getWebAuthnCredentials(browser, authId);
    console.log("da initialCredentials", initialCredentials.length);
    // Clean all credentials so that the one we add is the only one
    // const identity = await ECDSAKeyIdentity.generate({ extractable: true });
    // const keyPair = identity.getKeyPair();
    // const privateKey = await global["crypto"]["subtle"].exportKey(
    //   "pkcs8",
    //   keyPair.privateKey
    // );
    // const credential: WebAuthnCredential = {
    //   credentialId: randomUUID(),
    //   isResidentCredential: false,
    //   privateKey: uint8ArrayToBase64Url(new Uint8Array(privateKey)),
    //   signCount: 0,
    //   userHandle: "test",
    // };
    // await addWebAuthnCredential(
    //   browser,
    //   authId,
    //   credential,
    //   originToRelyingPartyId(II_URL)
    // );
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    const credentials = await getWebAuthnCredentials(browser, authId);
    // console.log(
    //   "da private key",
    //   uint8ArrayToBase64Url(new Uint8Array(privateKey))
    // );
    console.log("da authId", authId);
    console.log("credentials", credentials.length);
    console.log("credentials", credentials[0]);
    console.log("credentials", credentials[1]);

    const privateKeyBuffer = base64ToArrayBuffer(credentials[0].privateKey);

    const anonymousAgent = new HttpAgent({
      identity: new AnonymousIdentity(),
      host: REPLICA_URL,
    });
    await anonymousAgent.fetchRootKey();
    const iiAnonymousActor = Actor.createActor<_SERVICE>(
      internet_identity_idl,
      {
        agent: anonymousAgent,
        canisterId: II_CANISTER_ID,
      }
    );

    const response = await iiAnonymousActor.lookup(BigInt(userNumber));

    const publicKeyBuffer = response[0].pubkey;
    console.log(response);

    // Get the x and y coordinates of the public key
    // const x = publicKey.getX().toString("hex");
    // const y = publicKey.getY().toString("hex");
    // function hexToBase64Url(hexString: string) {
    //   // Step 1: Convert hex string to a buffer
    //   const buffer = Buffer.from(hexString, "hex");

    //   // Step 2: Convert buffer to a base64 string
    //   const base64String = buffer.toString("base64");

    //   // Step 3: Convert base64 string to a base64url string
    //   const base64UrlString = base64String
    //     .replace(/\+/g, "-") // Replace '+' with '-'
    //     .replace(/\//g, "_") // Replace '/' with '_'
    //     .replace(/=+$/, ""); // Remove trailing '='

    //   return base64UrlString;
    // }
    // const jwkPublicKey = {
    //   kty: "EC",
    //   crv: "P-256",
    //   // d: credentials[0].privateKey, // Example private key
    //   x: hexToBase64Url(x),
    //   y: hexToBase64Url(y),
    // };
    // const jwkKey = {
    //   ...jwkPublicKey,
    //   d: credentials[0].privateKey, // Example private key
    // };
    // console.log("privateKeyBuffer", privateKeyBuffer);

    const cryptoPrivateKey = await subtle.importKey(
      "raw",
      privateKeyBuffer,
      {
        name: "ECDSA",
        namedCurve: "P-256",
      },
      false,
      ["sign", "verify"]
    );
    // const publicKey = createPublicKey(credentials[0].privateKey);
    // const publicKeyDer = publicKey.export({ format: "der", type: "pkcs8" });
    const cryptoPublicKey = await subtle.importKey(
      "raw",
      new Uint8Array(publicKeyBuffer),
      {
        name: "ECDSA",
        namedCurve: "P-256",
      },
      false,
      ["verify"]
    );
    const identity = ECDSAKeyIdentity.fromKeyPair({
      privateKey: cryptoPrivateKey,
      publicKey: cryptoPublicKey,
    });

    const agent = new HttpAgent({
      identity,
      host: REPLICA_URL,
    });
    await agent.fetchRootKey();
    const iiActor = Actor.createActor<_SERVICE>(internet_identity_idl, {
      agent,
      canisterId: II_CANISTER_ID,
    });
    console.log("before da identity_info", BigInt(userNumber));

    const test = await iiActor.identity_info(BigInt(userNumber));
    console.log("da identity info", test);

    // await mainView.logout();
    // await FLOWS.loginAuthenticateView(userNumber, DEVICE_NAME1, browser);
  });
}, 300_000);
