import {
  authenticatedStore,
  authenticationStore,
} from "$lib/stores/authentication.store";
import { sessionStore } from "$lib/stores/session.store";
import { get } from "svelte/store";
import { isNullish, nonNullish } from "@dfinity/utils";
import type {
  AuthnMethodSecuritySettings,
  DeviceData,
  MetadataMapV2,
  UserNumber,
} from "$lib/generated/internet_identity_types";
import { MultiWebAuthnIdentity } from "$lib/utils/multiWebAuthnIdentity";
import { convertToValidCredentialData } from "$lib/utils/credential-devices";
import { DelegationChain, DelegationIdentity } from "@dfinity/identity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";

export class MigrationFlow {
  view = $state<"enterNumber" | "enterName" | "success">("enterNumber");
  identityNumber: UserNumber | undefined;

  constructor() {
    this.identityNumber = undefined;
  }

  authenticateWithIdentityNumber = async (
    identityNumber: UserNumber,
  ): Promise<void> => {
    this.identityNumber = identityNumber;
    const devices = await this.#lookupAuthenticators(identityNumber);
    const webAuthnAuthenticators = devices
      .filter(({ key_type }) => !("browser_storage_key" in key_type))
      .map(convertToValidCredentialData)
      .filter(nonNullish);
    const passkeyIdentity = MultiWebAuthnIdentity.fromCredentials(
      webAuthnAuthenticators,
      undefined,
      false,
    );
    const session = get(sessionStore);
    const delegation = await DelegationChain.create(
      passkeyIdentity,
      session.identity.getPublicKey(),
      new Date(Date.now() + 30 * 60 * 1000),
    );
    const identity = DelegationIdentity.fromDelegation(
      session.identity,
      delegation,
    );
    authenticationStore.set({ identity, identityNumber });
    this.view = "enterName";
  };

  createPasskey = async (name: string): Promise<void> => {
    if (isNullish(this.identityNumber)) {
      // Cannot call createPasskey before authenticateWithIdentityNumber.
      throw new Error("Identity number is null");
    }
    // TODO: Create the passkey in id.ai
    // TODO: Update identity name with call to canister. PENDING endpoint.
    const passkeyIdentity = await DiscoverablePasskeyIdentity.createNew(name);
    // The canister only allow for 50 characters, so for long domains we don't attach an origin
    // (those long domains are most likely a testnet with URL like <canister id>.large03.testnet.dfinity.network, and we basically only care about identity.ic0.app & identity.internetcomputer.org).
    const sanitizedOrigin =
      nonNullish(origin) && origin.length <= 50 ? origin : undefined;
    // Kick-off fetching "ua-parser-js";
    const uaParser = loadUAParser();
    const authenticatorAttachement =
      passkeyIdentity.getAuthenticatorAttachment();
    const deviceName = await inferPasskeyAlias({
      authenticatorType: authenticatorAttachement,
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: passkeyIdentity.getAaguid(),
    });
    const credentialId = passkeyIdentity.getCredentialId();
    if (isNullish(credentialId)) {
      throw new Error("Credential ID is null");
    }
    const securitySettings: AuthnMethodSecuritySettings = {
      protection: { Unprotected: null },
      purpose: { Authentication: null },
    };
    const metadata: MetadataMapV2 = [
      [
        "alias",
        {
          String: deviceName,
        },
      ],
    ];
    if (nonNullish(authenticatorAttachement)) {
      metadata.push([
        "authenticator_attachment",
        {
          String: authenticatorAttachement,
        },
      ]);
    }
    if (nonNullish(sanitizedOrigin)) {
      metadata.push([
        "origin",
        {
          String: sanitizedOrigin,
        },
      ]);
    }
    const response = await get(authenticatedStore).actor.authn_method_add(
      this.identityNumber,
      {
        security_settings: securitySettings,
        metadata,
        last_authentication: [],
        authn_method: {
          WebAuthn: {
            pubkey: Array.from(
              new Uint8Array(passkeyIdentity.getPublicKey().toDer()),
            ),
            credential_id: Array.from(new Uint8Array(credentialId)),
          },
        },
      },
    );
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: this.identityNumber,
      name,
      authMethod: {
        passkey: {
          credentialId: new Uint8Array(credentialId),
        },
      },
    });
    if ("Ok" in response) {
      this.view = "success";
    }
  };

  #lookupAuthenticators = async (
    userNumber: UserNumber,
  ): Promise<Omit<DeviceData, "alias">[]> => {
    const actor = await get(sessionStore).actor;
    const allDevices = await actor.lookup(userNumber);
    return allDevices.filter((device) => "authentication" in device.purpose);
  };
}
