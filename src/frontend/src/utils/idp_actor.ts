import {
  Actor,
  ActorSubclass,
  BinaryBlob,
  DerEncodedBlob,
  HttpAgent,
} from "@dfinity/agent";
import idp_idl from "../../generated/idp_idl";
import _SERVICE, {
  PublicKey,
  SessionKey,
  CredentialId,
  UserNumber,
  FrontendHostname,
  Timestamp,
} from "../../generated/idp_types";
import {
  tryLoadIdentity,
  authenticateFresh,
  persistIdentity,
} from "./handleAuthentication";
import {
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
  WebAuthnIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/agent";

const hostUrl = "http://dcs-messaging-13.dfinity.systems:8080/";

const canisterId: string = process.env.CANISTER_ID!;
export const baseActor = Actor.createActor<_SERVICE>(idp_idl, {
  agent: new HttpAgent({host: hostUrl}),
  canisterId,
});

export class IDPActor {
  private _actor?: ActorSubclass<_SERVICE>;
  private _chain?: DelegationChain;

  static create(): IDPActor {
    return new this(tryLoadIdentity());
  }

  protected constructor(public storedIdentity?: WebAuthnIdentity) {}

  public get userId(): UserNumber | undefined {
    const userId = localStorage.getItem("userId");
    return userId ? BigInt(userId) : undefined;
  }

  public set userId(userId: UserNumber | undefined) {
    if (userId !== undefined) {
      localStorage.setItem("userId", userId.toString());
    } else {
      localStorage.removeItem("userId");
    }
  }

  public get publicKey(): PublicKey {
    if (this.storedIdentity) {
      return Array.from(this.storedIdentity.getPublicKey().toDer());
    } else {
      throw new Error("getPublicKey: No stored identity found");
    }
  }

  public getCredentialId(): [] | [CredentialId] {
    if (this.storedIdentity) {
      return this.storedIdentity.rawId
        ? [Array.from(this.storedIdentity.rawId)]
        : [];
    } else {
      throw new Error("getCredentialId: No stored identity found");
    }
  }

  // Create a actor representing the backend using the stored identity
  // fails if is not there yet
  async getActor(): Promise<ActorSubclass<_SERVICE>> {
    for (const { delegation } of this._chain?.delegations || []) {
      // prettier-ignore
      if (+new Date(Number(delegation.expiration / BigInt(1000000))) <= +Date.now()) {
        this._actor = undefined;
        break;
      }
    }

    const storedIdentity = tryLoadIdentity();
    if (storedIdentity === undefined) {
      throw new Error("No identity were stored, but one is needed.");
    }

    if (this._actor === undefined) {
      // Create our actor with a DelegationIdentity to avoid re-prompting auth
      const sessionKey = Ed25519KeyIdentity.generate();
      const tenMinutesInMsec = 10 * 1000 * 60;
      this._chain = await DelegationChain.create(
        storedIdentity,
        sessionKey.getPublicKey(),
        new Date(Date.now() + tenMinutesInMsec),
        {
          targets: [Principal.from(canisterId)],
        }
      );

      const delegationIdentity = DelegationIdentity.fromDelegation(
        sessionKey,
        this._chain
      );

      const agent = new HttpAgent({ host: hostUrl, identity: delegationIdentity });
      this._actor = Actor.createActor<_SERVICE>(idp_idl, {
        agent,
        canisterId,
      });
    }

    return this._actor;
  }

  reconnect = async () => {
    localStorage.removeItem("identity");
    this._actor = undefined;
    this._chain = undefined;

    const identities = await baseActor.lookup(this.userId!!);
    for (const row of identities) {
      const { credential_id: credentialId, pubkey } = row;
      if (credentialId.length === 0) {
        continue;
      }

      console.log("row", row);
      // Strip DER header
      const strippedKey = pubkey.slice(19);
      const webAuthnJSON = {
        rawId: Buffer.from(credentialId[0]).toString("hex"),
        publicKey: Buffer.from(strippedKey).toString("hex"),
      };
      const identity = WebAuthnIdentity.fromJSON(JSON.stringify(webAuthnJSON));
      console.log({ identity });

      const sessionKey = Ed25519KeyIdentity.generate();
      const tenMinutesInMsec = 10 * 1000 * 60;
      try {
        this._chain = await DelegationChain.create(
          identity,
          sessionKey.getPublicKey(),
          new Date(Date.now() + tenMinutesInMsec),
          {
            targets: [Principal.from(canisterId)],
          }
        );
      } catch (err) {
        continue;
      }
      persistIdentity(identity);
      this.storedIdentity = identity;
      return;
    }
    throw Error("Couldn't find a registered device match");
  };

  register = async (alias: string) => {
    // user wants to register fresh, so always create new identity
    // also stores it in in the localStorage
    this._actor = undefined;
    this._chain = undefined;

    this.storedIdentity = await authenticateFresh();
    console.log("this.storedIdentity", this.storedIdentity);

    const credentialId = this.getCredentialId() ?? [];

    const actor = await this.getActor();
    console.log(
       `register(alias: ${alias}, publicKey: ${this.publicKey}, credentialId: ${credentialId})`
     );

    const userId = await actor.register({
      alias,
      pubkey: this.publicKey as PublicKey,
      credential_id: credentialId,
    });
    this.userId = userId;
    return userId;
  };

  add = async (
    userId: UserNumber,
    alias: string,
    newPublicKey: DerEncodedBlob,
    credentialId?: BinaryBlob
  ) => {
    const actor = await this.getActor();
    return await actor.add(userId, {
      alias,
      pubkey: Array.from(newPublicKey),
      credential_id: credentialId ? [Array.from(credentialId)] : [],
    });
  };

  remove = async (publicKey: PublicKey) => {
    const actor = await this.getActor();
    if (this.userId) {
      return await actor.remove(this.userId, publicKey);
    } else {
      throw new Error("no user was provided");
    }
  };

  lookup = async () => {
    if (this.userId) return baseActor.lookup(this.userId);
    else {
      throw new Error("Tried to lookup without a user present");
    }
  };

  prepareDelegation = async (
    hostname: FrontendHostname,
    sessionKey: SessionKey
  ) => {
    console.log(
       `prepare_delegation(user: ${this.userId}, hostname: ${hostname}, session_key: ${sessionKey})`
    );
    if (!!this.userId) {
      const actor = await this.getActor();
      return await actor.prepare_delegation(this.userId, hostname, sessionKey);
    }
    console.warn("Could not prepare delegation. User must authenticate first");
    return null;
  };

  getDelegation = async (
    hostname: FrontendHostname,
    sessionKey: SessionKey,
    timestamp: Timestamp
  ) => {
    console.log(
       `get_delegation(user: ${this.userId}, hostname: ${hostname}, session_key: ${sessionKey}, timestamp: ${timestamp})`
    );
    if (!!this.userId) {
      const actor = await this.getActor();
      return await actor.get_delegation(
        this.userId,
        hostname,
        sessionKey,
        timestamp
      );
    }
    console.warn("Could not get delegation. User must authenticate first");
    return null;
  };
}

const idp_actor = IDPActor.create();

export default idp_actor;
