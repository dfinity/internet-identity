import { SignedIdAlias } from "$generated/internet_identity_types";
import { idlFactory as vc_issuer_idl } from "$generated/vc_issuer_idl";

import { features } from "$src/features";
import { Actor, ActorSubclass, HttpAgent, Identity } from "@dfinity/agent";
import {
  CredentialSpec,
  Icrc21ConsentInfo,
  IssuedCredentialData,
  PreparedCredentialData,
  _SERVICE,
} from "@dfinity/internet-identity-vc-api";

import { inferHost } from "$src/utils/iiConnection";
import { Principal } from "@dfinity/principal";

// Boilerplate for contacting a canister implementing the Issuer API
export class VcIssuer {
  public constructor(readonly canisterId: Principal) {}

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
  }): Promise<PreparedCredentialData | "error"> => {
    const actor = await this.createActor(identity);

    const result = await actor.prepare_credential({
      signed_id_alias: signedIdAlias,
      credential_spec: credentialSpec,
    });

    if ("Err" in result) {
      console.error(
        "Could not prepare credential: " + JSON.stringify(result.Err)
      );
      return "error";
    }

    return result.Ok;
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
  }): Promise<IssuedCredentialData | "error"> => {
    const actor = await this.createActor(identity);

    const result = await actor.get_credential({
      signed_id_alias: signedIdAlias,
      prepared_context: preparedCredential.prepared_context,
      credential_spec: credentialSpec,
    });

    if ("Err" in result) {
      console.error("Could not get credential", result.Err);
      return "error";
    }

    return result.Ok;
  };

  getConsentMessage = async ({
    credentialSpec,
  }: {
    credentialSpec: CredentialSpec;
  }): Promise<Icrc21ConsentInfo | "error"> => {
    const actor = await this.createActor();

    const result = await actor.vc_consent_message({
      preferences: { language: "en-US" },
      credential_spec: credentialSpec,
    });

    if ("Err" in result) {
      console.error("Could not get consent message", result.Err);
      return "error";
    }

    return result.Ok;
  };

  getDerivationOrigin = async ({
    origin,
  }: {
    origin: string;
  }): Promise<{ kind: "origin"; origin: string } | { kind: "error" }> => {
    const actor = await this.createActor();

    let result;
    try {
      result = await actor.derivation_origin({
        frontend_hostname: origin,
      });
    } catch (e: unknown) {
      console.error("Could not get derivation origin (unexpected error)", e);
      return { kind: "error" };
    }

    if ("Err" in result) {
      console.error(
        "Could not get derivation origin (issuer error)",
        JSON.stringify(result.Err)
      );
      return { kind: "error" };
    }

    return { kind: "origin", origin: result.Ok.origin };
  };
}
