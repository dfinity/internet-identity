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
import { throwCanisterError } from "$lib/utils/utils";
import { toaster } from "$lib/components/utils/toaster";
import { findWebAuthnFlows, WebAuthnFlow } from "$lib/utils/findWebAuthnFlows";
import { supportsWebauthRoR } from "$lib/utils/userAgent";
import { canisterConfig } from "$lib/globals";
import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";

export class MigrationFlow {
  view = $state<"enterNumber" | "enterName">("enterNumber");
  identityNumber: UserNumber | undefined;
  authenticating = $state(false);
  #webAuthFlows: { flows: WebAuthnFlow[]; currentIndex: number } | undefined;

  constructor() {
    this.identityNumber = undefined;
  }

  authenticateWithIdentityNumber = async (
    identityNumber: UserNumber,
  ): Promise<void> => {
    this.authenticating = true;
    this.identityNumber = identityNumber;
    const devices = await this.#lookupAuthenticators(identityNumber);

    const webAuthnAuthenticators = devices
      .filter(({ key_type }) => !("browser_storage_key" in key_type))
      .map(convertToValidCredentialData)
      .filter(nonNullish);

    if (isNullish(this.#webAuthFlows)) {
      const flows = findWebAuthnFlows({
        supportsRor: supportsWebauthRoR(window.navigator.userAgent),
        devices: webAuthnAuthenticators,
        currentOrigin: window.location.origin,
        // Empty array is the same as no related origins.
        relatedOrigins: canisterConfig.related_origins[0] ?? [],
      });
      this.#webAuthFlows = {
        flows,
        currentIndex: 0,
      };
    }

    const flowsLength = this.#webAuthFlows?.flows.length ?? 0;
    // We reached the last flow. Start from the beginning.
    // This might happen if the user cancelled manually in the flow that would have been successful.
    if (this.#webAuthFlows?.currentIndex === flowsLength) {
      this.#webAuthFlows.currentIndex = 0;
    }
    const currentFlow = nonNullish(this.#webAuthFlows)
      ? this.#webAuthFlows.flows[this.#webAuthFlows.currentIndex]
      : undefined;

    const passkeyIdentity = MultiWebAuthnIdentity.fromCredentials(
      webAuthnAuthenticators,
      currentFlow?.rpId,
      currentFlow?.useIframe ?? false,
      // Set user verification to preferred to ensure the user is verified during migration.
      "preferred",
    );
    try {
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
    } catch (e: unknown) {
      if (isWebAuthnCancelError(e)) {
        // We only want to show a special error if the user might have to choose different web auth flow.
        if (nonNullish(this.#webAuthFlows) && flowsLength > 1) {
          // Increase the index to try the next flow.
          this.#webAuthFlows = {
            flows: this.#webAuthFlows.flows,
            currentIndex: this.#webAuthFlows.currentIndex + 1,
          };
          toaster.info({
            title: "Please try again",
            description:
              "The wrong domain was set for the passkey and the browser couldn't find it.",
          });
          return;
        }
      }

      throw new Error(
        "Failed to authenticate using passkey. Please try again and contact support if the issue persists.",
      );
    } finally {
      this.authenticating = false;
    }
  };

  createPasskey = async (name: string): Promise<void> => {
    if (isNullish(this.identityNumber)) {
      // Cannot call createPasskey before authenticateWithIdentityNumber.
      throw new Error("Identity number is null");
    }
    // TODO: Create the passkey in id.ai
    const passkeyIdentity = await DiscoverablePasskeyIdentity.createNew(name);
    const origin = window.location.origin;
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
    await Promise.all([
      get(authenticatedStore)
        .actor.authn_method_add(this.identityNumber, {
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
        })
        .then(throwCanisterError),
      get(authenticatedStore)
        .actor.identity_properties_replace(this.identityNumber, {
          name: [name],
        })
        .then(throwCanisterError),
    ]);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber: this.identityNumber,
      name,
      authMethod: {
        passkey: {
          credentialId: new Uint8Array(credentialId),
        },
      },
    });
    lastUsedIdentitiesStore.selectIdentity(this.identityNumber);
    toaster.success({
      title: "Migration completed successfully",
      duration: 4000,
    });
  };

  #lookupAuthenticators = async (
    userNumber: UserNumber,
  ): Promise<Omit<DeviceData, "alias">[]> => {
    const actor = await get(sessionStore).actor;
    const allDevices = await actor.lookup(userNumber);
    return allDevices.filter((device) => "authentication" in device.purpose);
  };
}
