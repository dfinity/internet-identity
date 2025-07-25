/**
 * This module provides an identity that is a bit like `WebAuthnIdentity`, but
 *
 * - You can create it from a _list_ of public keys/credential ids
 * - Upon signing, it will let the device sign with any of the given keys/credential ids.
 * - You need to use it to sign a message before you can use `getPublicKey`, because only
 *   then we know which one the user is actually using
 * - It doesn't support creating credentials; use `WebAuthnIdentity` for that
 */
import { webAuthnInIframe } from "$lib/legacy/flows/iframeWebAuthn";
import { PublicKey, Signature, SignIdentity } from "@dfinity/agent";
import { DER_COSE_OID, unwrapDER } from "@dfinity/identity";
import { isNullish, nonNullish } from "@dfinity/utils";
import borc from "borc";
import { CredentialData } from "./credential-devices";
import { bufferEqual } from "./iiConnection";
import { WebAuthnIdentity } from "./webAuthnIdentity";

/**
 * A SignIdentity that uses `navigator.credentials`. See https://webauthn.guide/ for
 * more information about WebAuthentication.
 */
export class MultiWebAuthnIdentity extends SignIdentity {
  /**
   * Create an identity from a JSON serialization.
   * @param json - json to parse
   */
  public static fromCredentials(
    credentialData: CredentialData[],
    rpId: string | undefined,
    iframe: boolean | undefined,
    userVerification: "discouraged" | "required" | "preferred" = "discouraged",
  ): MultiWebAuthnIdentity {
    return new this(credentialData, rpId, iframe, userVerification);
  }

  /* Set after the first `sign`, see `sign()` for more info. */
  protected _actualIdentity?: WebAuthnIdentity;

  protected constructor(
    readonly credentialData: CredentialData[],
    readonly rpId: string | undefined,
    readonly iframe: boolean | undefined,
    readonly userVerification:
      | "discouraged"
      | "required"
      | "preferred" = "discouraged",
  ) {
    super();
    this._actualIdentity = undefined;
  }

  /* This whole class is a bit of a hack since we can only consider it a
   * full-fledged "SignIdentity" after 'sign' was called, so we just hope
   * `getPublicKey()` is never called before `sign()`.
   */
  public getPublicKey(): PublicKey {
    if (isNullish(this._actualIdentity)) {
      throw new Error("cannot use getPublicKey() before a successful sign()");
    } else {
      return this._actualIdentity.getPublicKey();
    }
  }

  /* The use the 'sign'ing for two things:
   * - figure out the actual webauthn identity (pubkey) that the user uses
   * - actual signing
   * where the "actual signing" is done by passing the document to sign as the
   * "challenge" while getting (looking up) the (webauthn) credentials.
   *
   * After the first signing, we simply delegate the signing to the actual
   * WebAuthnIdentity created with the pubkey that we picked up during the
   * first signing.
   */
  public async sign(blob: ArrayBuffer): Promise<Signature> {
    if (this._actualIdentity) {
      return this._actualIdentity.sign(blob);
    }
    const options: CredentialRequestOptions = {
      publicKey: {
        allowCredentials: this.credentialData.map((cd) => ({
          type: "public-key",
          id: cd.credentialId,
        })),
        challenge: blob,
        userVerification: this.userVerification,
        rpId: this.rpId,
      },
    };
    const result = (
      this.iframe === true && nonNullish(this.rpId)
        ? await webAuthnInIframe(options)
        : await navigator.credentials.get(options)
    ) as PublicKeyCredential;

    for (const cd of this.credentialData) {
      if (bufferEqual(cd.credentialId, Buffer.from(result.rawId))) {
        this._actualIdentity = new WebAuthnIdentity(
          cd.credentialId,
          unwrapDER(cd.pubkey, DER_COSE_OID),
          undefined,
          this.rpId,
          undefined,
        );
        break;
      }
    }

    if (isNullish(this._actualIdentity)) {
      // Odd, user logged in with a credential we didn't provide?
      throw new Error("internal error");
    }

    const response = result.response as AuthenticatorAssertionResponse;
    const cbor = borc.encode(
      new borc.Tagged(55799, {
        authenticator_data: new Uint8Array(response.authenticatorData),
        client_data_json: new TextDecoder().decode(response.clientDataJSON),
        signature: new Uint8Array(response.signature),
      }),
    );
    // eslint-disable-next-line
    if (!cbor) {
      throw new Error("failed to encode cbor");
    }
    return new Uint8Array(cbor).buffer as Signature;
  }
}

export default {};
