import { Actor, ActorSubclass, HttpAgent } from "@dfinity/agent";
import {
  idlFactory as idp_idl,
  canisterId as idp_canister_id,
} from "dfx-generated/idp_service";
import _SERVICE, { UserId, Alias, PublicKey, CredentialId} from "../typings";
import { tryLoadIdentity, authenticate, authenticateFresh } from "./handleAuthentication";
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

  static create(
    userId = BigInt(1)
  ): IDPActor {
    return new this(tryLoadIdentity(), userId);
  }

  protected constructor(
    public storedIdentity?: WebAuthnIdentity,
    public userId?: bigint
  ) {
  }

  public getPublicKey() : PublicKey {
    if (this.storedIdentity) {
      return Array.from(this.storedIdentity.getPublicKey().toDer());
    } else {
      throw new Error("getPublicKey: No stored identity found");
    }
  }

  public getCredentialId() : [] | [CredentialId] {
    if (this.storedIdentity) {
      return this.storedIdentity.rawId
        ? [
            Array.from(
              new TextEncoder().encode(this.storedIdentity.rawId.toString())
            ),
          ]
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

  register = async (alias: Alias) => {

    // user wants to register fresh, so always create new identity
    // also stores it in in the localStorage
    this._actor = undefined;
    this._chain = undefined;

    this.storedIdentity = await authenticateFresh();
    console.log("this.storedIdentity", this.storedIdentity);

    const credentialId = this.getCredentialId() ?? [];

    const actor = await this.getActor();
    console.log(`register(alias: ${alias}, publicKey: ${this.getPublicKey()}, credentialId: ${credentialId})`);

    const userId = await actor.register(
      alias,
      this.getPublicKey() as PublicKey,
      credentialId
    );
    localStorage.setItem("userId", userId.toString());
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

  lookup = async (userId?: UserId) => {
    console.log(userId);
    const preferredUser = userId ?? this.userId;
    if (preferredUser) return baseActor.lookup(preferredUser);
    else {
      throw new Error("no user was provided");
    }
  };

  requestDelegation = async (publicKey?: PublicKey) => {
    const key = publicKey ?? this.getPublicKey();
    if (!!this.userId && !!key) {
      return await this._actor?.request_delegation(this.userId, key);
    }
    console.warn("Could not request delegation. User must authenticate first");
    return null;
  };

  getDelegation = async (publicKey?: PublicKey) => {
    const key = publicKey ?? this.getPublicKey();
    if (!!this.userId && !!key) {
      return await this._actor?.get_delegation(this.userId, key);
    }
    console.warn("Could not get delegation. User must authenticate first");
    return null;
  };
}

const savedUserId = localStorage.getItem("userId");
const userId = savedUserId ? BigInt(savedUserId) : undefined;
const idp_actor = IDPActor.create(userId);

export default idp_actor;
