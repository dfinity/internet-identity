import { AuthClient, IdbStorage } from "@dfinity/auth-client";
import { AnonymousIdentity, SignIdentity } from "@dfinity/agent";
import type {
  DerEncodedPublicKey,
  PublicKey,
  Signature,
} from "@dfinity/agent/lib/esm/auth";

export class ChallengeAuthClient extends AuthClient {
  public constructor(challenge: string) {
    super(
      new AnonymousIdentity(),
      new ChallengeIdentity(challenge),
      null,
      new IdbStorage(),
      undefined,
      undefined,
    );
  }
}

export class ChallengeIdentity extends SignIdentity {
  private readonly challenge: ArrayBuffer;
  public constructor(challenge: string) {
    super();
    this.challenge = base64ToBytes(challenge);
  }

  getPublicKey(): PublicKey {
    return new Challenge(this.challenge);
  }

  sign(_blob: ArrayBuffer): Promise<Signature> {
    throw new Error("not implemented");
  }
}

export class Challenge {
  constructor(public challenge: ArrayBuffer) {}

  public toDer(): DerEncodedPublicKey {
    return this.challenge;
  }
}

function base64ToBytes(base64: string): Uint8Array {
  const binString = atob(base64);
  return Uint8Array.from(binString, (m) => m.codePointAt(0)!);
}
