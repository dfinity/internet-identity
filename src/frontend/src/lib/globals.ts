import { Principal } from "@icp-sdk/core/principal";
import { type _SERVICE } from "$lib/generated/internet_identity_types";
import { type InternetIdentityFrontendInit } from "$lib/generated/internet_identity_frontend_types";
import { readCanisterId } from "$lib/utils/init";
import {
  Actor,
  ActorSubclass,
  HttpAgent,
  HttpAgentOptions,
} from "@icp-sdk/core/agent";
import { inferHost } from "$lib/utils/iiConnection";
import { idlFactory as internetIdentityIDL } from "$lib/generated/internet_identity_idl";
import { init as internetIdentityFrontendInit } from "$lib/generated/internet_identity_frontend_idl";
import { IDL } from "@icp-sdk/core/candid";
import { fromBase64 } from "./utils/utils";

// IDL definition for decoding `InternetIdentitySynchronizedConfig` from the backend canister.
//
// This is intentionally maintained separately from the backend implementation to prevent
// direct dependencies between frontend and backend, as they may be deployed independently.
//
// Only compatibility between the two is guaranteed, not strict synchronization.
const OpenIdEmailVerificationIDL = IDL.Variant({
  Google: IDL.Null,
  Unknown: IDL.Null,
  Microsoft: IDL.Null,
});

const backendCanisterConfigIDL = IDL.Record({
  openid_configs: IDL.Opt(
    IDL.Vec(
      IDL.Record({
        auth_uri: IDL.Text,
        jwks_uri: IDL.Text,
        logo: IDL.Text,
        name: IDL.Text,
        fedcm_uri: IDL.Opt(IDL.Text),
        email_verification: IDL.Opt(OpenIdEmailVerificationIDL),
        issuer: IDL.Text,
        auth_scope: IDL.Vec(IDL.Text),
        client_id: IDL.Text,
      }),
    ),
  ),
});

// Types for above IDL definition
export type OpenIdEmailVerification =
  | { Google: null }
  | { Unknown: null }
  | { Microsoft: null };
export interface OpenIdConfig {
  auth_uri: string;
  jwks_uri: string;
  logo: string;
  name: string;
  fedcm_uri: [] | [string];
  email_verification: [] | [OpenIdEmailVerification];
  issuer: string;
  auth_scope: Array<string>;
  client_id: string;
}
export type BackendCanisterConfig = {
  openid_configs: [] | [OpenIdConfig[]];
};

export let canisterId: Principal;
export let frontendCanisterConfig: InternetIdentityFrontendInit;
export let backendCanisterConfig: BackendCanisterConfig;
export let agentOptions: HttpAgentOptions;
export let anonymousAgent: HttpAgent;
export let anonymousActor: ActorSubclass<_SERVICE>;

export const initGlobals = async () => {
  canisterId = Principal.fromText(readCanisterId());

  // Read frontend config from body tag
  const base64Config = document.body.dataset.canisterConfig;
  if (base64Config === undefined) {
    throw new Error("Frontend canister config is missing from the page");
  }
  [frontendCanisterConfig] = IDL.decode(
    internetIdentityFrontendInit({ IDL }),
    fromBase64(base64Config),
  ) as unknown as [InternetIdentityFrontendInit];

  // Fetch backend config from the backend canister
  const response = await fetch(
    `${frontendCanisterConfig.backend_origin}/.config.did.bin`,
  );
  [backendCanisterConfig] = IDL.decode(
    [backendCanisterConfigIDL],
    new Uint8Array(await response.arrayBuffer()),
  ) as unknown as [BackendCanisterConfig];

  agentOptions = {
    host: inferHost(),
    shouldFetchRootKey: frontendCanisterConfig.fetch_root_key[0] ?? false,
  };
  anonymousAgent = HttpAgent.createSync(agentOptions);
  // Fetch subnet keys to speed up queries during authentication,
  // this avoids having to fetch them later on user interaction.
  void anonymousAgent.fetchSubnetKeys(canisterId);
  anonymousActor = Actor.createActor<_SERVICE>(internetIdentityIDL, {
    agent: anonymousAgent,
    canisterId,
  });
};

// Get primary origin (either https://id.ai or https://beta.id.ai) when deployed on beta or prod
export const getPrimaryOrigin = () =>
  frontendCanisterConfig.related_origins[0]?.find((origin) =>
    origin.endsWith("id.ai"),
  );
