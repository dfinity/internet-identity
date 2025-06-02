import { type Signature } from "@dfinity/agent";
import { Ed25519KeyIdentity } from "@dfinity/identity";
import {
  CosePublicKey,
  DiscoverablePasskeyIdentity,
} from "./discoverablePasskeyIdentity";

const dummyIdentity = Ed25519KeyIdentity.generate(new Uint8Array(32));

export class DiscoverableDummyIdentity extends DiscoverablePasskeyIdentity {
  constructor() {
    super();
  }

  getPublicKey(): CosePublicKey {
    return CosePublicKey.fromDer(dummyIdentity.getPublicKey().toDer());
  }

  getCredentialId(): ArrayBuffer | undefined {
    return new Uint8Array(32);
  }

  getAaguid(): string | undefined {
    return;
  }

  getName(): string | undefined {
    return;
  }

  getAuthenticatorAttachment(): AuthenticatorAttachment | undefined {
    return;
  }

  sign(blob: ArrayBuffer): Promise<Signature> {
    return dummyIdentity.sign(blob);
  }
}
