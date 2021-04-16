import { Actor, ActorSubclass, blobFromUint8Array, blobToHex, HttpAgent } from "@dfinity/agent";
import {
  idlFactory as idp_idl,
  canisterId as idp_canister_id,
} from "dfx-generated/idp_service";
import _SERVICE, { UserId, Alias, PublicKey, CredentialId } from "../typings";
import { tryLoadIdentity, authenticate, authenticateFresh, persistIdentity } from "./handleAuthentication";
import {
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
  WebAuthnIdentity,
} from "@dfinity/identity";
import { Principal } from "@dfinity/agent";

export const baseActor = Actor.createActor<_SERVICE>(idp_idl, {
  agent: new HttpAgent(),
  canisterId: idp_canister_id,
});

export class IDPActor {
  private _actor?: ActorSubclass<_SERVICE>;
  private _chain?: DelegationChain;

  static create(): IDPActor {
    return new this(tryLoadIdentity());
  }

  protected constructor(
    public storedIdentity?: WebAuthnIdentity,
  ) {
  }

  public get userId(): UserId | undefined {
    const userId = localStorage.getItem("userId");
    return userId ? BigInt(userId) : undefined;
  }

  public set userId(userId : UserId | undefined) {
    if (userId !== undefined) {
      localStorage.setItem("userId", userId.toString())
    } else {
      localStorage.removeItem("userId")
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
          targets: [Principal.from(idp_canister_id)],
        }
      );

      const delegationIdentity = DelegationIdentity.fromDelegation(
        sessionKey,
        this._chain
      );

      const agent = new HttpAgent({ identity: delegationIdentity });
      this._actor = Actor.createActor<_SERVICE>(idp_idl, {
        agent,
        canisterId: idp_canister_id,
      });
    }

    return this._actor;
  }

  reconnect = async () => {
    localStorage.removeItem("identity");
    this._actor = undefined;
    this._chain = undefined;

    const identities = await baseActor.lookup(this.userId!!)
    for (const row of identities) {
      const [alias, publicKey, expiry, credentialId] = row;
      if (credentialId.length === 0) {
        continue;
      }

      console.log('row', row)
      // Strip DER header
      const strippedKey = publicKey.slice(19);
      const identity = WebAuthnIdentity.fromJSON(JSON.stringify({
        rawId: blobToHex(blobFromUint8Array(Buffer.from(credentialId[0]))),
        publicKey: blobToHex(blobFromUint8Array(Buffer.from(strippedKey)))
      }))

      console.log('identity', identity)

      const sessionKey = Ed25519KeyIdentity.generate();
      const tenMinutesInMsec = 10 * 1000 * 60;
      try {
        this._chain = await DelegationChain.create(
          identity,
          sessionKey.getPublicKey(),
          new Date(Date.now() + tenMinutesInMsec),
          {
            targets: [Principal.from(idp_canister_id)],
          }
        );
      } catch (err) {
        continue;
      }
      persistIdentity(identity)
      this.storedIdentity = identity;
      return
    }
    throw Error("Couldn't find a registered device match")
  }

  register = async (alias: Alias) => {

    // user wants to register fresh, so always create new identity
    // also stores it in in the localStorage
    this._actor = undefined;
    this._chain = undefined;

    this.storedIdentity = await authenticateFresh();
    console.log("this.storedIdentity", this.storedIdentity);

    const credentialId = this.getCredentialId() ?? [];

    const actor = await this.getActor();
    console.log(`register(alias: ${alias}, publicKey: ${this.publicKey}, credentialId: ${credentialId})`);

    const userId = await actor.register(
      alias,
      this.publicKey as PublicKey,
      credentialId
    );
    this.userId = userId
  };

  add = async (userId: UserId, alias: Alias, credentialId?: string) => {
    const identity = this.storedIdentity ?? (await authenticate());
    const publicKey = Array.from(identity.getPublicKey().toDer());
    const actor = await this.getActor();
    return await actor.add(
      userId,
      alias,
      publicKey,
      credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
    );
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

  requestDelegation = async (publicKey?: PublicKey) => {
    console.log(`request_delegation()`);
    const key = publicKey ?? this.publicKey;
    if (!!this.userId && !!key) {
      return await this._actor?.request_delegation(this.userId, key);
    }
    console.warn("Could not request delegation. User must authenticate first");
    return null;
  };

  getDelegation = async (publicKey?: PublicKey) => {
    const key = publicKey ?? this.publicKey;
    console.log(`get_delegation(pubkey: ${key})`);
    if (!!this.userId && !!key) {
      return await this._actor?.get_delegation(this.userId, key);
    }
    console.warn("Could not get delegation. User must authenticate first");
    return null;
  };
}

const idp_actor = IDPActor.create();

export default idp_actor;
