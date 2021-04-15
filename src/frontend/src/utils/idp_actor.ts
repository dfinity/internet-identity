import { Actor, BinaryBlob, DerEncodedBlob, HttpAgent } from "@dfinity/agent";
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

  register = async (alias: Alias, credentialId?: string) => {
    console.log(`register(alias: ${alias}`);
    const identity = await authenticate();
    const publicKey = Array.from(identity.getPublicKey().toDer());
    return this.actor_with_identity(identity).register(
      alias,
      publicKey,
      credentialId ? [Array.from(new TextEncoder().encode(credentialId))] : []
    );
  };

  add = async (userId: UserId, alias: Alias, newPublicKey: DerEncodedBlob, credentialId?: BinaryBlob) => {
    const identity = await authenticate();
    return this.actor_with_identity(identity)
      .add(
        userId,
        alias,
        Array.from(newPublicKey),
        credentialId ? [Array.from(credentialId)] : []
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
    return baseActor.lookup(userId);
  };

  get_delegation = (userId: UserId, identity: WebAuthnIdentity) => {
    const publicKey = Array.from(identity.getPublicKey().toDer());
    console.log(`get_delegation(user_id = ${userId}, publicKey: ${publicKey}`);
    return baseActor.get_delegation(userId, publicKey);
  };
}

const idp_actor = new IDPActor();
export default idp_actor;
