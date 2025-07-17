import { extractAAGUID } from "$lib/utils/webAuthn";
import {
  DER_COSE_OID,
  DerEncodedPublicKey,
  PublicKey,
  SignIdentity,
  Signature,
  fromHex,
  toHex,
  wrapDER,
} from "@dfinity/agent";
import { bufFromBufLike } from "@dfinity/candid";
import { isNullish } from "@dfinity/utils";
import { randomBytes } from "@noble/hashes/utils";
import borc from "borc";

/**
 * This whole file was copied agent-js:
 * https://github.com/dfinity/agent-js/blob/main/packages/identity/src/identity/webauthn.ts#L206
 *
 * We need some changes to implement some of our new features: ROR and Discoverable Passkeys.
 *
 * We will move it back to agent-js once the changes are stable.
 */

function _coseToDerEncodedBlob(cose: ArrayBuffer): DerEncodedPublicKey {
  return wrapDER(cose, DER_COSE_OID).buffer as DerEncodedPublicKey;
}

type PublicKeyCredentialWithAttachment = Omit<PublicKeyCredential, "toJSON"> & {
  // Extends `PublicKeyCredential` with an optional field introduced in the WebAuthn level 3 spec:
  // https://w3c.github.io/webauthn/#dom-publickeycredential-authenticatorattachment
  // Already supported by Chrome, Safari and Edge
  // Note: `null` is included here as a possible value because Edge set this value to null in the
  // past.
  authenticatorAttachment: AuthenticatorAttachment | undefined | null;
};

/**
 * From the documentation;
 * The authData is a byte array described in the spec. Parsing it will involve slicing bytes from
 * the array and converting them into usable objects.
 *
 * See https://webauthn.guide/#registration (subsection "Example: Parsing the authenticator data").
 * @param authData The authData field of the attestation response.
 * @returns The COSE key of the authData.
 */
function _authDataToCose(authData: ArrayBuffer): ArrayBuffer {
  const dataView = new DataView(new ArrayBuffer(2));
  const idLenBytes = authData.slice(53, 55);
  [...new Uint8Array(idLenBytes)].forEach((v, i) => dataView.setUint8(i, v));
  const credentialIdLength = dataView.getUint16(0);

  // Get the public key object.
  return authData.slice(55 + credentialIdLength);
}

export class CosePublicKey implements PublicKey {
  protected _encodedKey: DerEncodedPublicKey;

  public constructor(protected _cose: ArrayBuffer) {
    this._encodedKey = _coseToDerEncodedBlob(_cose);
  }

  public toDer(): DerEncodedPublicKey {
    return this._encodedKey;
  }

  public getCose(): ArrayBuffer {
    return this._cose;
  }
}

/**
 * Create a challenge from a string or array. The default challenge is always the same
 * because we don't need to verify the authenticity of the key on the server (we don't
 * register our keys with the IC). Any challenge would do, even one per key, randomly
 * generated.
 * @param challenge The challenge to transform into a byte array. By default a hard
 *        coded string.
 */
function _createChallengeBuffer(
  challenge: string | Uint8Array = "<ic0.app>",
): Uint8Array {
  if (typeof challenge === "string") {
    return Uint8Array.from(challenge, (c) => c.charCodeAt(0));
  } else {
    return challenge;
  }
}

/**
 * Create a credentials to authenticate with a server. This is necessary in order in
 * WebAuthn to get credentials IDs (which give us the public key and allow us to
 * sign), but in the case of the Internet Computer, we don't actually need to register
 * it, so we don't.
 * @param credentialCreationOptions an optional CredentialCreationOptions object
 */
async function _createCredential(
  credentialCreationOptions?: CredentialCreationOptions,
): Promise<PublicKeyCredentialWithAttachment | null> {
  const creds = (await navigator.credentials.create(
    credentialCreationOptions ?? {
      publicKey: {
        authenticatorSelection: {
          userVerification: "preferred",
        },
        attestation: "direct",
        challenge: _createChallengeBuffer(),
        pubKeyCredParams: [
          { type: "public-key", alg: PubKeyCoseAlgo.ECDSA_WITH_SHA256 },
        ],
        rp: {
          name: "Internet Identity Service",
        },
        user: {
          id: randomBytes(16),
          name: "Internet Identity",
          displayName: "Internet Identity",
        },
      },
    },
  )) as PublicKeyCredentialWithAttachment | null;

  if (creds === null) {
    return null;
  }

  return {
    // do _not_ use ...creds here, as creds is not enumerable in all cases
    id: creds.id,
    response: creds.response,
    type: creds.type,
    authenticatorAttachment: creds.authenticatorAttachment,
    getClientExtensionResults: creds.getClientExtensionResults,
    // Some password managers will return a Uint8Array, so we ensure we return an ArrayBuffer.
    rawId: bufFromBufLike(creds.rawId),
  };
}

// See https://www.iana.org/assignments/cose/cose.xhtml#algorithms for a complete
// list of these algorithms. We only list the ones we support here.
enum PubKeyCoseAlgo {
  ECDSA_WITH_SHA256 = -7,
}

/**
 * A SignIdentity that uses `navigator.credentials`. See https://webauthn.guide/ for
 * more information about WebAuthentication.
 */
export class WebAuthnIdentity extends SignIdentity {
  /**
   * Create an identity from a JSON serialization.
   * @param json - json to parse
   */
  public static fromJSON(json: string): WebAuthnIdentity {
    const { publicKey, rawId, rpId } = JSON.parse(json);

    if (
      typeof publicKey !== "string" ||
      typeof rawId !== "string" ||
      (typeof rpId !== "string" && rpId !== undefined)
    ) {
      throw new Error("Invalid JSON string.");
    }

    return new this(
      fromHex(rawId),
      fromHex(publicKey),
      undefined,
      rpId,
      undefined,
    );
  }

  /**
   * Create an identity.
   * @param credentialCreationOptions an optional CredentialCreationOptions Challenge
   */
  public static async create(
    credentialCreationOptions?: CredentialCreationOptions,
  ): Promise<WebAuthnIdentity> {
    const creds = await _createCredential(credentialCreationOptions);

    if (!creds || creds.type !== "public-key") {
      throw new Error("Could not create credentials.");
    }

    const response = creds.response as AuthenticatorAttestationResponse;
    if (response.attestationObject === undefined) {
      throw new Error("Was expecting an attestation response.");
    }

    // Parse the attestationObject as CBOR.
    const attObject = borc.decodeFirst(
      new Uint8Array(response.attestationObject),
    );

    return new this(
      creds.rawId,
      _authDataToCose(attObject.authData),
      creds.authenticatorAttachment ?? undefined,
      credentialCreationOptions?.publicKey?.rp.id,
      extractAAGUID(attObject.authData),
    );
  }

  protected _publicKey: CosePublicKey;

  public constructor(
    public readonly rawId: ArrayBuffer,
    cose: ArrayBuffer,
    protected authenticatorAttachment: AuthenticatorAttachment | undefined,
    protected rpId: string | undefined,
    public readonly aaguid: string | undefined,
  ) {
    super();
    this._publicKey = new CosePublicKey(cose);
  }

  public getPublicKey(): PublicKey {
    return this._publicKey;
  }

  /**
   * WebAuthn level 3 spec introduces a new attribute on successful WebAuthn interactions,
   * see https://w3c.github.io/webauthn/#dom-publickeycredential-authenticatorattachment.
   * This attribute is already implemented for Chrome, Safari and Edge.
   *
   * Given the attribute is only available after a successful interaction, the information is
   * provided opportunistically and might also be `undefined`.
   */
  public getAuthenticatorAttachment(): AuthenticatorAttachment | undefined {
    return this.authenticatorAttachment;
  }

  public async sign(blob: ArrayBuffer): Promise<Signature> {
    const result = (await navigator.credentials.get({
      publicKey: {
        allowCredentials: [
          {
            type: "public-key",
            id: this.rawId,
          },
        ],
        challenge: blob,
        userVerification: "preferred",
        rpId: this.rpId,
      },
    })) as PublicKeyCredentialWithAttachment;

    if (result.authenticatorAttachment !== null) {
      this.authenticatorAttachment = result.authenticatorAttachment;
    }

    const response = result.response as AuthenticatorAssertionResponse;

    const cbor = borc.encode(
      new borc.Tagged(55799, {
        authenticator_data: new Uint8Array(response.authenticatorData),
        client_data_json: new TextDecoder().decode(response.clientDataJSON),
        signature: new Uint8Array(response.signature),
      }),
    );
    if (isNullish(cbor)) {
      throw new Error("failed to encode cbor");
    }
    return cbor.buffer as Signature;
  }

  /**
   * Allow for JSON serialization of all information needed to reuse this identity.
   */
  public toJSON(): JsonnableWebAuthnIdentity {
    return {
      publicKey: toHex(this._publicKey.getCose()),
      rawId: toHex(this.rawId),
      rpId: this.rpId,
    };
  }
}

/**
 * ReturnType<WebAuthnIdentity.toJSON>
 */
export interface JsonnableWebAuthnIdentity {
  // The hexadecimal representation of the DER encoded public key.
  publicKey: string;
  // The string representation of the local WebAuthn Credential.id (base64url encoded).
  rawId: string;
  // The RP ID of the WebAuthn Credential.
  rpId: string | undefined;
}
