import { Actor, HttpAgent } from "@dfinity/agent";
import {
  idlFactory as idp_idl,
  canisterId as idp_canister_id,
} from "dfx-generated/idp_service";
import _SERVICE, { UserId, Alias, PublicKey, CredentialId } from "../typings";
import { authenticate } from "./handleAuthentication";
import { WebAuthnIdentity } from "@dfinity/identity";

const agent = new HttpAgent();
export const baseActor = Actor.createActor<_SERVICE>(idp_idl, {
  agent,
  canisterId: idp_canister_id,
});

export class IDPActor {
  actor: _SERVICE;
  constructor(overrideActor?: _SERVICE) {
    this.actor = overrideActor ?? baseActor;
  }
  register = async (userId: UserId, alias: Alias, credentialId?: string) => {
    console.log("Registering user " + userId + " with alias " + alias);
    const identity = await authenticate();
    const key = Array.from(new TextEncoder().encode(identity.publicKey));
    return this.actor.register(
      userId,
      alias,
      key,
      credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
    );
  };
  add = async (userId: UserId, alias: Alias, credentialId?: string) => {
    const identity = await authenticate();
    const key = Array.from(new TextEncoder().encode(identity.publicKey));
    return this.actor
      .add(
        userId,
        alias,
        key,
        credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
      )
      .then(async () => {
        const update = await this.actor.lookup(userId);
        debugger;
      });
  };
  remove = async (userId: UserId) => {
    const identity = await authenticate();
    const key = Array.from(new TextEncoder().encode(identity.publicKey));
    return this.actor.remove(userId, key);
  };
  lookup = (userId: UserId) => {
    console.log(userId);
    return this.actor.lookup(userId);
  };
  get_delegation = (userId: UserId, identity: WebAuthnIdentity) => {
    const publicKey = Array.from(new TextEncoder().encode(identity.toJSON().publicKey));
    console.log("get_delegation(user_id = " + userId + ", publicKey: " + publicKey);
    return this.actor.get_delegation(userId, publicKey);
  };
}

const idp_actor = new IDPActor();
export default idp_actor;
