import { Actor, ActorSubclass, HttpAgent } from "@dfinity/agent";
import {
  idlFactory as idp_idl,
  canisterId as idp_canister_id,
} from "dfx-generated/idp_service";
import _SERVICE, { UserId, Alias, PublicKey } from "../typings";
import { authenticate } from "./handleAuthentication";
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

  publicKey?: PublicKey;
  credentialId?: [] | [number[]];

  static create(
    storedIdentity?: WebAuthnIdentity,
    userId = BigInt(1)
  ): IDPActor {
    return new this(storedIdentity, userId);
  }

  protected constructor(
    public storedIdentity?: WebAuthnIdentity,
    public userId?: bigint
  ) {
    if (storedIdentity) {
      this.publicKey = Array.from(storedIdentity.getPublicKey().toDer());
      this.credentialId = storedIdentity.rawId
        ? [
            Array.from(
              new TextEncoder().encode(storedIdentity.rawId.toString())
            ),
          ]
        : [];
    }
  }

  async getActor(anonymousOkay = false): Promise<ActorSubclass<_SERVICE>> {
    for (const { delegation } of this._chain?.delegations || []) {
      // prettier-ignore
      if (+new Date(Number(delegation.expiration / BigInt(1000000))) <= +Date.now()) {
          this._actor = undefined;
          break;
        }
    }

    const storedIdentity = this.storedIdentity;
    if (storedIdentity === undefined) {
      if (anonymousOkay) {
        return Actor.createActor<_SERVICE>(idp_idl, {
          agent: new HttpAgent(),
          canisterId: idp_canister_id,
        });
      } else {
        throw new Error("No identity were stored, but one is needed.");
      }
    }

    if (this._actor === undefined) {
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
    const identity = this.storedIdentity ?? (await authenticate());
    // need to create an actor at this stage

    console.log(`register(alias: ${alias}`);

    const credentialId = this.credentialId ?? [];

    const actor = await this.getActor(true);

    const userId = await actor.register(
      alias,
      this.publicKey as PublicKey,
      credentialId
    );
    localStorage.setItem("userId", userId.toString());
  };

  add = async (userId: UserId, alias: Alias, credentialId?: string) => {
    const identity = this.storedIdentity ?? (await authenticate());
    const publicKey = Array.from(identity.getPublicKey().toDer());
    const actor = await this.getActor();
    await actor.add(
      userId,
      alias,
      publicKey,
      credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
    );

    const update = await actor.lookup(userId);
  };

  remove = async (publicKey: PublicKey) => {
    const actor = await this.getActor();
    if (this.userId) {
      return await actor.remove(this.userId, publicKey);
    } else {
      throw new Error("no user was provided");
    }
  };

  lookup = (userId?: UserId) => {
    console.log(userId);
    const preferredUser = userId || this.userId;
    if (preferredUser) return baseActor.lookup(preferredUser);
    else {
      throw new Error("no user was provided");
    }
  };

  get_delegation = async (userId: UserId, identity: WebAuthnIdentity) => {
    const publicKey = Array.from(identity.getPublicKey().toDer());
    console.log(`get_delegation(user_id = ${userId}, publicKey: ${publicKey}`);
    const actor = await this.getActor();
    return await actor.get_delegation(userId, publicKey);
  };
}
declare global {
  interface Window {
    idp_actor: any;
  }
}

const storedIdentity = localStorage.getItem("identity");
const savedUserId = localStorage.getItem("userId");
const userId = savedUserId ? BigInt(savedUserId) : undefined;
const idp_actor = IDPActor.create(
  storedIdentity ? WebAuthnIdentity.fromJSON(storedIdentity) : undefined,
  userId
);

export default idp_actor;
