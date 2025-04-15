import {
  bufFromBufLike,
  type DerEncodedPublicKey,
  type PublicKey,
  type Signature,
  SignIdentity,
} from "@dfinity/agent";
import { DER_COSE_OID, unwrapDER, wrapDER } from "@dfinity/identity";
import borc from "borc";
import { isNullish, nonNullish } from "@dfinity/utils";
import { extractAAGUID } from "$lib/utils/webAuthn";

/**
 * From the documentation;
 * The authData is a byte array described in the spec. Parsing it will involve slicing bytes from
 * the array and converting them into usable objects.
 *
 * See https://webauthn.guide/#registration (subsection "Example: Parsing the authenticator data").
 * @param authData The authData field of the attestation response.
 * @returns The COSE key of the authData.
 */
export function authDataToCose(authData: ArrayBuffer): ArrayBuffer {
  const dataView = new DataView(new ArrayBuffer(2));
  const idLenBytes = authData.slice(53, 55);
  [...new Uint8Array(idLenBytes)].forEach((v, i) => dataView.setUint8(i, v));
  const credentialIdLength = dataView.getUint16(0);

  // Get the public key object.
  return authData.slice(55 + credentialIdLength);
}

function coseToDerEncodedBlob(cose: ArrayBuffer): DerEncodedPublicKey {
  return wrapDER(cose, DER_COSE_OID).buffer as DerEncodedPublicKey;
}

function coseFromDerEncodedBlob(derEncoded: DerEncodedPublicKey): ArrayBuffer {
  return unwrapDER(derEncoded, DER_COSE_OID).buffer as ArrayBuffer;
}

export class CosePublicKey implements PublicKey {
  protected _encodedKey: DerEncodedPublicKey;

  public constructor(protected _cose: ArrayBuffer) {
    this._encodedKey = coseToDerEncodedBlob(_cose);
  }

  public toDer(): DerEncodedPublicKey {
    return this._encodedKey;
  }

  public getCose(): ArrayBuffer {
    return this._cose;
  }

  static fromDer(derEncodedBlob: ArrayBuffer): CosePublicKey {
    return new CosePublicKey(coseFromDerEncodedBlob(derEncodedBlob));
  }

  static fromCose(cose: ArrayBuffer): CosePublicKey {
    return new CosePublicKey(cose);
  }
}

/**
 * Parse credentials to authenticate with a server. This is necessary in order in
 * WebAuthn to get credentials IDs (which give us the public key and allow us to
 * sign), but in the case of the Internet Computer, we don't actually need to register
 * it, so we don't.
 * @param credential The credential created previously
 */
function parseCredential(
  credential: Credential | null,
): PublicKeyCredentialWithAttachment | null {
  const creds = credential as PublicKeyCredentialWithAttachment | null;

  if (creds === null) {
    return null;
  }

  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore This type error is probably also in agent-js
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

type PublicKeyCredentialWithAttachment = PublicKeyCredential & {
  // Extends `PublicKeyCredential` with an optional field introduced in the WebAuthn level 3 spec:
  // https://w3c.github.io/webauthn/#dom-publickeycredential-authenticatorattachment
  // Already supported by Chrome, Safari and Edge
  // Note: `null` is included here as a possible value because Edge set this value to null in the
  // past.
  authenticatorAttachment: AuthenticatorAttachment | undefined | null;
  response: AuthenticatorAssertionResponse & {
    attestationObject?: ArrayBuffer;
  };
};

type CredentialCreationOptionsWithoutChallenge = Omit<
  CredentialCreationOptions,
  "publicKey"
> & {
  publicKey: Omit<PublicKeyCredentialCreationOptions, "challenge">;
};

type CredentialRequestOptionsWithoutChallenge = Omit<
  CredentialRequestOptions,
  "publicKey"
> & {
  publicKey: Omit<PublicKeyCredentialRequestOptions, "challenge">;
};

const publicKeyFromResult = (
  result: PublicKeyCredentialWithAttachment,
): Promise<CosePublicKey> => {
  if (result.response.attestationObject === undefined) {
    throw new Error("Was expecting an attestation response.");
  }

  // Parse the attestationObject as CBOR.
  const attObject = borc.decodeFirst(
    new Uint8Array(result.response.attestationObject),
  );
  return Promise.resolve(new CosePublicKey(authDataToCose(attObject.authData)));
};

// See https://www.iana.org/assignments/cose/cose.xhtml#algorithms for a complete
// list of these algorithms. We only list the ones we support here.
enum PubKeyCoseAlgo {
  ECDSA_WITH_SHA256 = -7,
}

export class DiscoverablePasskeyIdentity extends SignIdentity {
  #credentialCreationOptions?: CredentialCreationOptionsWithoutChallenge;
  #credentialRequestOptions?: CredentialRequestOptionsWithoutChallenge;
  #authenticatorAttachment?: AuthenticatorAttachment;
  #credentialId?: ArrayBuffer;
  #aaguid?: string;
  #publicKey?: CosePublicKey;

  #getPublicKey: (
    result: PublicKeyCredentialWithAttachment,
  ) => Promise<CosePublicKey>;

  constructor({
    getPublicKey = publicKeyFromResult,
    credentialCreationOptions,
    credentialRequestOptions,
  }: {
    getPublicKey?: (
      result: PublicKeyCredentialWithAttachment,
    ) => Promise<CosePublicKey>;
    credentialCreationOptions?: CredentialCreationOptionsWithoutChallenge;
    credentialRequestOptions?: CredentialRequestOptionsWithoutChallenge;
  } = {}) {
    super();
    this.#getPublicKey = getPublicKey;
    this.#credentialCreationOptions = credentialCreationOptions;
    this.#credentialRequestOptions = credentialRequestOptions;
  }

  static async create(
    credentialCreationOptions?: CredentialCreationOptionsWithoutChallenge,
  ): Promise<DiscoverablePasskeyIdentity> {
    const identity = new DiscoverablePasskeyIdentity({
      credentialCreationOptions,
    });
    await identity.sign(
      Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)).buffer,
    );
    return identity;
  }

  getPublicKey(): CosePublicKey {
    if (!this.#publicKey) {
      throw Error("Sign first to retrieve public key");
    }
    return this.#publicKey;
  }

  getCredentialId(): ArrayBuffer | undefined {
    return this.#credentialId;
  }

  getAaguid(): string | undefined {
    return this.#aaguid;
  }

  getName(): string | undefined {
    return this.#credentialCreationOptions?.publicKey.user.displayName;
  }

  getAuthenticatorAttachment(): AuthenticatorAttachment | undefined {
    return this.#authenticatorAttachment;
  }

  async sign(blob: ArrayBuffer): Promise<Signature> {
    const credential = await (this.#credentialCreationOptions &&
    !this.#credentialId
      ? navigator.credentials.create({
          ...this.#credentialCreationOptions,
          publicKey: {
            ...this.#credentialCreationOptions.publicKey,
            challenge: blob,
            pubKeyCredParams: [
              { type: "public-key", alg: PubKeyCoseAlgo.ECDSA_WITH_SHA256 },
            ],
            extensions: {
              ...this.#credentialCreationOptions.publicKey.extensions,
              // return credential properties / passkey info
              credProps: true,
            },
          },
        })
      : this.#credentialRequestOptions
        ? navigator.credentials.get({
            ...this.#credentialRequestOptions,
            publicKey: {
              ...this.#credentialRequestOptions.publicKey,
              challenge: blob,
            },
          })
        : navigator.credentials.get({
            mediation: "optional",
            publicKey: {
              rpId: window.location.hostname, // TODO: This probably must be set to same value as set during credential creation
              challenge: blob,
              userVerification: "preferred",
            },
          }));

    const result = parseCredential(credential);

    if (result === null) {
      throw Error("Woops");
    }

    if (result.authenticatorAttachment !== null) {
      this.#authenticatorAttachment = result.authenticatorAttachment;
    }
    this.#credentialId = result.rawId;
    if (!this.#publicKey) {
      this.#publicKey = await this.#getPublicKey(result);
    }
    const cbor = borc.encode(
      new borc.Tagged(55799, {
        authenticator_data: new Uint8Array(result.response.authenticatorData),
        client_data_json: new TextDecoder()
          .decode(result.response.clientDataJSON)
          .replace("webauthn.create", "webauthn.get"),
        signature: new Uint8Array(result.response.signature),
      }),
    );
    if (isNullish(cbor)) {
      throw new Error("failed to encode cbor");
    }
    if (nonNullish(result.response.attestationObject)) {
      // Parse the attestationObject as CBOR.
      const attObject = borc.decodeFirst(
        new Uint8Array(result.response.attestationObject),
      );
      this.#aaguid = extractAAGUID(attObject.authData);
    }
    return cbor.buffer as Signature;
  }
}
