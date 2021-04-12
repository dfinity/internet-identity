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
  constructor(actor: _SERVICE) {
    this.actor = actor;
  }
  retister = (
    userId: UserId,
    alias: Alias,
    publicKey: PublicKey,
    credentialId?: CredentialId
  ) => {
    return this.actor.register(
      userId,
      alias,
      publicKey,
      credentialId ? [credentialId] : []
    );
  };
  add = (
    userId: UserId,
    alias: Alias,
    publicKey: PublicKey,
    credentialId?: CredentialId
  ) => {
    return this.actor.add(
      userId,
      alias,
      publicKey,
      credentialId ? [credentialId] : []
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
