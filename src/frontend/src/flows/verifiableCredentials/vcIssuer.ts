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

// Boilerplate for contacting a canister implementing the Issuer API
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
  }): Promise<
    | { kind: "origin"; origin: string }
    | { kind: "use_default" }
    | { kind: "error" }
  > => {
    const actor = await this.createActor();

    const methodName = "derivation_origin" as const;

    let result;
    try {
      result = await actor[methodName]({
        frontend_hostname: origin,
      });
    } catch (e: unknown) {
      try {
        // XXX: this "ensures" that the error is the one expected by the issuer spec.
        // This unfortunately depends on the exact method name, only works for update calls and
        // heavily relies on the format of agent-js' errors. The safest bet hence seems to be
        // to convert the error to string and perform a match.
        // eslint-disable-next-line
        const str = (e as any).toString();
        const isMethodUndefined =
          str.match(`has no.*method '${methodName}'`) !== null;
        if (!isMethodUndefined) {
          console.error(e);
          throw e;
        }
      } catch {
        console.error(e);
        throw e;
      }
      return { kind: "use_default" };
    }

    if ("Err" in result) {
      console.error("Could not get derivation origin", result.Err);
      return { kind: "error" };
    }

    return { kind: "origin", origin: result.Ok.origin };
  };
}
