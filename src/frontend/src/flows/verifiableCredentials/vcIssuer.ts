import { SignedIdAlias } from "$generated/internet_identity_types";
import { idlFactory as vc_issuer_idl } from "$generated/vc_issuer_idl";
import {
  CredentialSpec,
  IssuedCredentialData,
  PreparedCredentialData,
  _SERVICE,
} from "$generated/vc_issuer_types";
import { features } from "$src/features";
import { Actor, ActorSubclass, HttpAgent } from "@dfinity/agent";

import { inferHost } from "$src/utils/iiConnection";

export class VcIssuer {
  public constructor(readonly canisterId: string) {}

  // Create an actor representing the backend
  createActor = async (): Promise<ActorSubclass<_SERVICE>> => {
    const agent = new HttpAgent({
      // TODO: should the agent ever be authenticated?
      host: inferHost(),
    });

    // Only fetch the root key when we're not in prod
    if (features.FETCH_ROOT_KEY) {
      await agent.fetchRootKey();
    }
    const actor = Actor.createActor<_SERVICE>(vc_issuer_idl, {
      agent,
      canisterId: this.canisterId,
    });
    return actor;
  };

  prepareCredential = async ({
    signedIdAlias,
    credentialSpec,
  }: {
    signedIdAlias: SignedIdAlias;
    credentialSpec: CredentialSpec;
  }): Promise<PreparedCredentialData | { error: string }> => {
    const actor = await this.createActor();

    const result = await actor.prepare_credential({
      signed_id_alias: signedIdAlias,
      credential_spec: credentialSpec,
    });

    // TODO: proper error handling
    if ("Err" in result) {
      console.error("wops");
      return { error: "wops" };
    }

    return result.ok;
  };

  getCredential = async ({
    signedIdAlias,
    preparedCredential,
    credentialSpec,
  }: {
    signedIdAlias: SignedIdAlias;
    credentialSpec: CredentialSpec;
    preparedCredential: PreparedCredentialData;
  }): Promise<IssuedCredentialData | { error: string }> => {
    const actor = await this.createActor();

    const result = await actor.get_credential({
      signed_id_alias: signedIdAlias,
      prepared_context: preparedCredential.prepared_context,
      credential_spec: credentialSpec,
    });

    // TODO: proper error handling
    if ("Err" in result) {
      console.error("wops");
      return { error: "wops" };
    }

    return result.ok;
  };
}
