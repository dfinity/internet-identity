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
export function authDataToCose(authData: Uint8Array): Uint8Array {
  const view = new DataView(authData.buffer);
  const coseKey = authData.slice(55 + view.getUint16(53, false));
  const decoded = borc.decodeFirst(coseKey);
  const cleaned = new Map();
  for (const [key, value] of decoded.entries()) {
    if (typeof key === "number") {
      // Only keep numeric keys, removing WebAuthn extension keys as a result
      cleaned.set(key, value);
    }
  }
  return borc.encode(cleaned);
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
  credential: Credential,
): PublicKeyCredentialWithAttachment {
  const creds = credential as PublicKeyCredentialWithAttachment;

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
  return Promise.resolve(
    new CosePublicKey(
      authDataToCose(new Uint8Array(attObject.authData)).buffer,
    ),
  );
};

// See https://www.iana.org/assignments/cose/cose.xhtml#algorithms for a complete
// list of these algorithms. We only list the ones we support here.
enum PubKeyCoseAlgo {
  ECDSA_WITH_SHA256 = -7,
  ED25519 = -8,
  RSA_WITH_SHA256 = -257,
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

  static async createNew(name: string): Promise<DiscoverablePasskeyIdentity> {
    const identity = new DiscoverablePasskeyIdentity({
      credentialCreationOptions: creationOptions(name),
    });
    await identity.sign(
      Uint8Array.from("<ic0.app>", (c) => c.charCodeAt(0)).buffer,
    );
    return identity;
  }

  static useExisting({
    credentialId,
    getPublicKey,
  }: {
    credentialId?: Uint8Array;
    getPublicKey: (
      result: PublicKeyCredentialWithAttachment,
    ) => Promise<CosePublicKey>;
  }): DiscoverablePasskeyIdentity {
    return new DiscoverablePasskeyIdentity({
      getPublicKey,
      credentialRequestOptions: requestOptions(credentialId),
    });
  }

  getPublicKey(): PublicKey {
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
    const credential = await (nonNullish(this.#credentialCreationOptions)
      ? navigator.credentials.create({
          ...this.#credentialCreationOptions,
          publicKey: {
            ...this.#credentialCreationOptions.publicKey,
            challenge: blob,
          },
        })
      : nonNullish(this.#credentialRequestOptions)
        ? navigator.credentials.get({
            ...this.#credentialRequestOptions,
            publicKey: {
              ...this.#credentialRequestOptions.publicKey,
              challenge: blob,
            },
          })
        : Promise.reject(new Error("Missing credential options")));

    if (credential === null) {
      throw Error("WebAuthn credential is missing");
    }

    const result = parseCredential(credential);

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
        client_data_json: new TextDecoder().decode(
          result.response.clientDataJSON,
        ),
        signature: new Uint8Array(result.response.signature),
      }),
    );
    if (isNullish(cbor)) {
      throw new Error("Failed to encode cbor");
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

export const creationOptions = (
  name: string,
): CredentialCreationOptionsWithoutChallenge => ({
  publicKey: {
    // Identify the AAGUID of the passkey provider
    attestation: "direct",
    authenticatorSelection: {
      // For maximum compatibility with various passkey provider,
      // should not be set to either platform or cross-platform.
      authenticatorAttachment: undefined,
      // Require passkeys to verify the user e.g. TouchID
      userVerification: "required",
      // Require passkeys to be discoverable
      residentKey: "required",
      requireResidentKey: true,
    },
    // extensions: {
    //   credProps: true,
    // },
    // Algorithms supported by the Internet Computer
    pubKeyCredParams: [
      { type: "public-key", alg: PubKeyCoseAlgo.ECDSA_WITH_SHA256 },
      { type: "public-key", alg: PubKeyCoseAlgo.ED25519 },
      { type: "public-key", alg: PubKeyCoseAlgo.RSA_WITH_SHA256 },
    ],
    // How II will identify itself
    rp: {
      name: "Internet Identity Service",
    },
    // User id is set to a random value since it's not used by II,
    // the passkey is created before the actual user is created.
    user: {
      id: window.crypto.getRandomValues(new Uint8Array(16)),
      displayName: name,
      name,
    },
  },
});

export const requestOptions = (
  credentialId?: Uint8Array,
): CredentialRequestOptionsWithoutChallenge => ({
  publicKey: {
    // Either use the specified credential id or let the user pick a passkey
    allowCredentials: nonNullish(credentialId)
      ? [{ id: credentialId, type: "public-key" }]
      : undefined,
    // Require passkeys to verify the user e.g. TouchID
    userVerification: "required",
  },
});
