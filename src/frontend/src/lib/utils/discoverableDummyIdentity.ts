import { PublicKey, type Signature } from "@dfinity/agent";
import { Ed25519KeyIdentity } from "@dfinity/identity";
import { DiscoverablePasskeyIdentity } from "./discoverablePasskeyIdentity";
import { canisterConfig } from "$lib/globals";
import { isNullish } from "@dfinity/utils";

const getSeedIndex = (): bigint => {
  if (canisterConfig.dummy_auth[0]?.[0]?.prompt_for_index === true) {
    const value = prompt("Enter seed index", "0")?.trim();
    if (isNullish(value)) {
      throw new Error("Operation cancelled");
    }
    const index = BigInt(value);
    if (index < BigInt(0)) {
      throw new Error("Invalid index");
    }
    return index;
  }
  return BigInt(0);
};

export class DiscoverableDummyIdentity extends DiscoverablePasskeyIdentity {
  #name?: string;
  #identity: Ed25519KeyIdentity;
  #credentialId: Uint8Array;

  constructor(name?: string) {
    super();
    this.#name = name;

    const index = getSeedIndex();
    const buffer = new ArrayBuffer(32);
    const view = new DataView(buffer);
    view.setBigUint64(0, index);
    const seed = new Uint8Array(buffer);
    this.#identity = Ed25519KeyIdentity.generate(seed);
    this.#credentialId = seed;
  }

  static createNew(name: string): Promise<DiscoverableDummyIdentity> {
    return Promise.resolve(new DiscoverableDummyIdentity(name));
  }

  static useExisting(): DiscoverableDummyIdentity {
    return new DiscoverableDummyIdentity();
  }

  getPublicKey(): PublicKey {
    return this.#identity.getPublicKey();
  }

  getCredentialId(): ArrayBuffer | undefined {
    return this.#credentialId;
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
    return this.#identity.sign(blob);
  }
}
