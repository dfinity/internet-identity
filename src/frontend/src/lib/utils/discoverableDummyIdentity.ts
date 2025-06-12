import { PublicKey, type Signature } from "@dfinity/agent";
import { Ed25519KeyIdentity } from "@dfinity/identity";
import { DiscoverablePasskeyIdentity } from "./discoverablePasskeyIdentity";

const dummyIdentity = Ed25519KeyIdentity.generate(new Uint8Array(32));

export class DiscoverableDummyIdentity extends DiscoverablePasskeyIdentity {
  #name?: string;

  constructor(name?: string) {
    super();
    this.#name = name;
  }

  getPublicKey(): PublicKey {
    return dummyIdentity.getPublicKey();
  }

  getCredentialId(): ArrayBuffer | undefined {
    return new Uint8Array(32);
  }

  getAaguid(): string | undefined {
    return;
  }

  getName(): string | undefined {
    return this.#name;
  }

  getAuthenticatorAttachment(): AuthenticatorAttachment | undefined {
    return;
  }

  sign(blob: ArrayBuffer): Promise<Signature> {
    return dummyIdentity.sign(blob);
  }
}
