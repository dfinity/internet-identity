import { Actor, HttpAgent, Identity } from "@dfinity/agent";
import {
  idlFactory as idp_idl,
  canisterId as idp_canister_id,
} from "dfx-generated/idp_service";
import _SERVICE, { UserId, Alias, PublicKey, CredentialId } from "../typings";
import { authenticate } from "./handleAuthentication";
import { WebAuthnIdentity } from "@dfinity/identity";

export const baseActor = Actor.createActor<_SERVICE>(idp_idl, {
  agent: new HttpAgent(),
  canisterId: idp_canister_id,
});

export class IDPActor {
  actor?: _SERVICE;
  constructor() {
    this.actor = undefined;
  }

  actor_with_identity = (identity: WebAuthnIdentity) => {
      if (this.actor === undefined) {
        const agent = new HttpAgent({identity});
        this.actor = Actor.createActor<_SERVICE>(idp_idl, {
          agent,
          canisterId: idp_canister_id,
        });
      }
      return this.actor;
  }

  register = async (userId: UserId, alias: Alias, credentialId?: string) => {
    console.log(`register(user_id = ${userId}, alias: ${alias}`);
    const identity = await authenticate();
    const publicKey = Array.from(identity.getPublicKey().toDer());
    return this.actor_with_identity(identity).register(
      userId,
      alias,
      publicKey,
      credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
    );
  };

  add = async (userId: UserId, alias: Alias, credentialId?: string) => {
    const identity = await authenticate();
    const publicKey = Array.from(identity.getPublicKey().toDer());
    return this.actor_with_identity(identity)
      .add(
        userId,
        alias,
        publicKey,
        credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
      )
      .then(async () => {
        const update = await this.actor_with_identity(identity).lookup(userId);
        debugger;
      });
  };

  remove = async (userId: UserId) => {
    const identity = await authenticate();
    const publicKey = Array.from(identity.getPublicKey().toDer());
    return this.actor_with_identity(identity).remove(userId, publicKey);
  };

  lookup = (userId: UserId) => {
    console.log(userId);
    const actor = baseActor;
    return actor.lookup(userId);
  };

  get_delegation = (userId: UserId, identity: WebAuthnIdentity) => {
    const publicKey = Array.from(identity.getPublicKey().toDer());
    console.log(`get_delegation(user_id = ${userId}, publicKey: ${publicKey}`);
    return this.actor_with_identity(identity).get_delegation(userId, publicKey);
  };
}

const idp_actor = new IDPActor();
export default idp_actor;
