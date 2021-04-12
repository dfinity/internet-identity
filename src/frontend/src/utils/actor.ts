import { Actor, HttpAgent } from "@dfinity/agent";
import {
  idlFactory as idp_idl,
  canisterId as idp_canister_id,
} from "dfx-generated/idp_service";
import _SERVICE, { UserId, Alias, PublicKey, CredentialId } from "../typings";

const agent = new HttpAgent();
export const actor = Actor.createActor<_SERVICE>(idp_idl, {
  agent,
  canisterId: idp_canister_id,
});

class IDPActor {
  actor: _SERVICE;
  constructor(overrideActor?: _SERVICE) {
    this.actor = overrideActor ?? actor;
  }
  register = (
    userId: UserId,
    alias: Alias,
    publicKey: PublicKey,
    credentialId?: string
  ) => {
    return this.actor.register(
      userId,
      alias,
      publicKey,
      credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
    );
  };
  add = (
    userId: UserId,
    alias: Alias,
    publicKey: PublicKey,
    credentialId?: string
  ) => {
    return this.actor.add(
      userId,
      alias,
      publicKey,
      credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
    );
  };
  remove = (userId: UserId, publicKey: PublicKey) => {
    return this.actor.remove(userId, publicKey);
  };
  lookup = (userId: UserId) => {
    return this.actor.lookup(userId);
  };
}

export default IDPActor;
