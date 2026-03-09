import { test } from "../fixtures";
import { expect } from "@playwright/test";
import {
  addVirtualAuthenticator,
  dummyAuth,
  getRandomIndex,
  II_URL,
  LEGACY_II_URL,
} from "../utils";
import { toSeed } from "../fixtures/identity";
import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import borc from "borc";
import {
  DER_COSE_OID,
  DerEncodedPublicKey,
  PublicKey,
  unwrapDER,
  wrapDER,
} from "@icp-sdk/core/agent";

type PublicKeyCredentialWithAttachment = PublicKeyCredential & {
  authenticatorAttachment: AuthenticatorAttachment | undefined | null;
  response: AuthenticatorAttestationResponse;
};

class CosePublicKey implements PublicKey {
  protected _encodedKey: DerEncodedPublicKey;

  public constructor(protected _cose: Uint8Array) {
    this._encodedKey = coseToDerEncodedBlob(_cose);
  }

  public toDer(): DerEncodedPublicKey {
    return this._encodedKey;
  }

  public getCose(): Uint8Array {
    return this._cose;
  }

  static fromDer(derEncodedBlob: Uint8Array): CosePublicKey {
    return new CosePublicKey(coseFromDerEncodedBlob(derEncodedBlob));
  }

  static fromCose(cose: Uint8Array): CosePublicKey {
    return new CosePublicKey(cose);
  }
}

function coseToDerEncodedBlob(cose: Uint8Array): DerEncodedPublicKey {
  return wrapDER(cose, DER_COSE_OID) as DerEncodedPublicKey;
}

function coseFromDerEncodedBlob(derEncoded: DerEncodedPublicKey): Uint8Array {
  return unwrapDER(derEncoded, DER_COSE_OID);
}

function authDataToCose(authData: Uint8Array): Uint8Array {
  if (authData.byteLength < 55) {
    throw new Error(
      "Invalid authData: too short to contain attested credential data.",
    );
  }

  const view = new DataView(
    authData.buffer,
    authData.byteOffset,
    authData.byteLength,
  );
  const credentialIdLength = view.getUint16(53, false);
  const coseStart = 55 + credentialIdLength;

  if (authData.byteLength < coseStart) {
    throw new Error(
      "Invalid authData: credential ID length exceeds authData size.",
    );
  }

  const coseKey = authData.slice(coseStart);
  const decoded = borc.decodeFirst(
    coseKey.buffer.slice(
      coseKey.byteOffset,
      coseKey.byteOffset + coseKey.byteLength,
    ),
  );
  const cleaned = new Map();
  for (const [key, value] of decoded.entries()) {
    if (typeof key === "number") {
      // Only keep numeric keys, removing WebAuthn extension keys as a result
      cleaned.set(key, value);
    }
  }
  return borc.encode(cleaned);
}

const publicKeyFromResult = (result: {
  response: { attestationObject?: ArrayBuffer };
}): Promise<CosePublicKey> => {
  if (result.response.attestationObject === undefined) {
    throw new Error("Was expecting an attestation response.");
  }

  // Parse the attestationObject as CBOR.
  const attestationBytes = new Uint8Array(result.response.attestationObject);
  const attObject = borc.decodeFirst(
    attestationBytes.buffer.slice(
      attestationBytes.byteOffset,
      attestationBytes.byteOffset + attestationBytes.byteLength,
    ),
  );
  return Promise.resolve(
    new CosePublicKey(authDataToCose(new Uint8Array(attObject.authData))),
  );
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function parseRegistrationCredential(json: any): {
  id: string;
  rawId: ArrayBuffer;
  type: PublicKeyCredentialType;
  response: AuthenticatorAttestationResponse;
} {
  const base64urlToBuffer = (base64url: string): ArrayBuffer => {
    const base64 = base64url.replace(/-/g, "+").replace(/_/g, "/");
    const pad = base64.length % 4;
    const padded = base64 + (pad !== 0 ? "=".repeat(4 - pad) : "");
    const binary = atob(padded);
    const bytes = new Uint8Array(binary.length);

    for (let i = 0; i < binary.length; i++) {
      bytes[i] = binary.charCodeAt(i);
    }

    return bytes.buffer;
  };

  return {
    id: json.id,
    rawId: base64urlToBuffer(json.rawId),
    type: json.type,
    response: {
      clientDataJSON: base64urlToBuffer(json.response.clientDataJSON),
      attestationObject: base64urlToBuffer(json.response.attestationObject),
    } as AuthenticatorAttestationResponse,
  };
}

test.describe("Upgrade flow", () => {
  const LEGACY_PASSKEY_NAME = "pre-upgrade-passkey";

  // Create legacy identity and navigate to upgrade page
  test.beforeEach(
    async ({ page, identities, actorForIdentity, replaceAuthForIdentity }) => {
      // Navigate to legacy page that doesn't redirect
      await page.goto(LEGACY_II_URL + "/self-service");

      // Add virtual authenticator to be able to create a legacy passkey using navigator.credentials.create in the page context
      await addVirtualAuthenticator(page);

      // Create a passkey using navigator.credentials.create and serialize it
      const result = parseRegistrationCredential(
        await page.evaluate(async () => {
          const result = (await navigator.credentials.create({
            publicKey: {
              challenge: Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)),
              attestation: "direct",
              pubKeyCredParams: [
                { type: "public-key", alg: -7 },
                { type: "public-key", alg: -8 },
                { type: "public-key", alg: -257 },
              ],
              rp: {
                name: "Internet Identity Service",
              },
              user: {
                id: window.crypto.getRandomValues(new Uint8Array(16)),
                displayName: "Internet Identity User",
                name: "Internet Identity User",
              },
            },
          })) as PublicKeyCredentialWithAttachment;

          result.toJSON();

          return result.toJSON();
        }),
      );
      const publicKey = await publicKeyFromResult(result);

      // Use an actor to create a legacy passkey (not id.ai)
      // since this functionality is no longer available.
      const actor = await actorForIdentity(identities[0].identityNumber);

      // Add the legacy passkey to the identity
      await actor.authn_method_add(identities[0].identityNumber, {
        metadata: [
          ["alias", { String: LEGACY_PASSKEY_NAME }],
          ["origin", { String: LEGACY_II_URL }],
        ],
        authn_method: {
          WebAuthn: {
            pubkey: publicKey.getCose(),
            credential_id: new Uint8Array(result.rawId),
            aaguid: [],
          },
        },
        security_settings: {
          protection: { Unprotected: null },
          purpose: { Authentication: null },
        },
        last_authentication: [],
      });

      // Remove the dummy auth so the identity is only accessible via the legacy passkey,
      // which simulates the state of an identity that hasn't been upgraded yet.
      await actor.authn_method_remove(
        identities[0].identityNumber,
        new Uint8Array(
          Ed25519KeyIdentity.generate(toSeed(identities[0].dummyAuthIndex))
            .getPublicKey()
            .toDer(),
        ),
      );

      // Generate a new dummy auth index for identity
      replaceAuthForIdentity(identities[0].identityNumber, getRandomIndex());

      // Navigate to new II_URL
      await page.goto(II_URL);

      // await manageRecoveryPage.goto();
      // await signInWithIdentity(page, identities[0].identityNumber);
      // await manageRecoveryPage.assertNotActivated();
      // words.current = await manageRecoveryPage.activate(async (wizard) => {
      //   await wizard.acknowledge();
      //   const recoveryPhrase = await wizard.writeDown();
      //   await wizard.verifySelecting(recoveryPhrase);
      //   return recoveryPhrase;
      // });
      // await manageRecoveryPage.assertActivated();
      // await recoveryPage.goto();
    },
  );

  test("can upgrade identity", async ({ page, identities, managePage }) => {
    const dialog = page.getByRole("dialog");
    await page.getByRole("button", { name: "Sign in" }).click();
    await expect(dialog).toBeVisible();
    await dialog.getByRole("button", { name: "Continue with passkey" }).click();
    await dialog.getByRole("button", { name: "Upgrade" }).click();
    await dialog
      .getByPlaceholder("Internet Identity number")
      .fill(identities[0].identityNumber.toString());
    await dialog.getByRole("button", { name: "Continue" }).click();
    await dialog.getByLabel("Identity name").fill(identities[0].name);
    dummyAuth(identities[0].dummyAuthIndex)(page);
    await dialog.getByRole("button", { name: "Upgrade identity" }).click();
    await managePage.assertVisible();
  });
});
