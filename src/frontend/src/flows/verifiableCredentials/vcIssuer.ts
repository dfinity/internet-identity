import { SignedIdAlias } from "$generated/internet_identity_types";
import { idlFactory as vc_issuer_idl } from "$generated/vc_issuer_idl";
import {
  CredentialSpec,
  Icrc21ConsentInfo,
  IssuedCredentialData,
  PreparedCredentialData,
  _SERVICE,
} from "$generated/vc_issuer_types";
import { features } from "$src/features";
import { Actor, ActorSubclass, HttpAgent, Identity } from "@dfinity/agent";

import { inferHost } from "$src/utils/iiConnection";

export class VcIssuer {
  public constructor(readonly canisterId: string) {}

  // Create an actor representing the backend
  createActor = async (
    identity?: Identity
  ): Promise<ActorSubclass<_SERVICE>> => {
    const agent = new HttpAgent({
      host: inferHost(),
      identity,
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
    identity,
  }: {
    signedIdAlias: SignedIdAlias;
    credentialSpec: CredentialSpec;
    identity: Identity;
  }): Promise<PreparedCredentialData | { error: string }> => {
    const actor = await this.createActor(identity);

    const result = await actor.prepare_credential({
      signed_id_alias: signedIdAlias,
      credential_spec: credentialSpec,
    });

    // TODO: proper error handling
    if ("err" in result) {
      console.error("wops", result);
      return { error: "wops" };
    }

    return result.ok;
  };

  getCredential = async ({
    signedIdAlias,
    preparedCredential,
    credentialSpec,
    identity,
  }: {
    signedIdAlias: SignedIdAlias;
    credentialSpec: CredentialSpec;
    preparedCredential: PreparedCredentialData;
    identity: Identity;
  }): Promise<IssuedCredentialData | { error: string }> => {
    const actor = await this.createActor(identity);

    const result = await actor.get_credential({
      signed_id_alias: signedIdAlias,
      prepared_context: preparedCredential.prepared_context,
      credential_spec: credentialSpec,
    });

    // TODO: proper error handling
    if ("err" in result) {
      console.error("wops");
      return { error: "wops" };
    }

    return result.ok;
  };

  getConsentMessage = async ({
    credentialSpec,
  }: {
    credentialSpec: CredentialSpec;
  }): Promise<Icrc21ConsentInfo | { error: string }> => {
    const actor = await this.createActor();

    const result = await actor.vc_consent_message({
      preferences: { language: "en-US" },
      credential_spec: credentialSpec,
    });

    // TODO: proper error handling
    if ("err" in result) {
      console.error("wops", result);
      return { error: "wops" };
    }

    return result.ok;
  };
}
