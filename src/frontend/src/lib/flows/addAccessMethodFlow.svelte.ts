import { anonymousActor, canisterConfig } from "$lib/globals";
import { get } from "svelte/store";
import { isNullish, nonNullish } from "@dfinity/utils";
import {
  createAnonymousNonce,
  createGoogleRequestConfig,
  decodeJWTWithNameAndEmail,
  requestJWT,
} from "$lib/utils/openID";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { secureRandomId, throwCanisterError, waitFor } from "$lib/utils/utils";
import type {
  AuthnMethodData,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { features } from "$lib/legacy/features";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
import { bufferEqual } from "$lib/utils/iiConnection";

const POLL_INTERVAL = 3000; // Should be frequent enough
const REGISTRATION_ID_LENGTH = 5;

export interface AddAccessMethodFlowOptions {
  isMaxOpenIdCredentialsReached?: boolean;
}

export class AddAccessMethodFlow {
  #waitingForDevice?: AuthnMethodData;

  view = $state<
    | "chooseMethod"
    | "addPasskey"
    | "continueOnAnotherDevice"
    | "confirmationCode"
    | "continueOnNewDevice"
  >("chooseMethod");
  isSystemOverlayVisible = $state(false);
  isRegistrationWindowOpen = $state(false);
  isRegistrationWindowPassed = $state(false);
  newDeviceLink?: URL;

  constructor(options?: AddAccessMethodFlowOptions) {
    if (options?.isMaxOpenIdCredentialsReached === true) {
      this.view = "addPasskey";
    }
  }

  linkGoogleAccount = async (): Promise<OpenIdCredential> => {
    const { identity, actor, identityNumber } = get(authenticatedStore);

    const clientId = canisterConfig.openid_google?.[0]?.[0]?.client_id;
    if (isNullish(clientId)) {
      throw new Error("Google is not configured");
    }
    try {
      const requestConfig = createGoogleRequestConfig(clientId);
      this.isSystemOverlayVisible = true;
      const { nonce, salt } = await createAnonymousNonce(
        identity.getPrincipal(),
      );
      const jwt = await requestJWT(requestConfig, {
        nonce,
        mediation: "required",
      });
      const { iss, sub, aud, name, email } = decodeJWTWithNameAndEmail(jwt);
      this.isSystemOverlayVisible = false;
      await actor
        .openid_credential_add(identityNumber, jwt, salt)
        .then(throwCanisterError);
      return {
        aud,
        iss,
        sub,
        metadata: [
          ["name", { String: name }],
          ["email", { String: email }],
        ],
        last_usage_timestamp: [],
      };
    } finally {
      this.isSystemOverlayVisible = false;
    }
  };

  createPasskey = async (): Promise<AuthnMethodData> => {
    const { selected } = get(lastUsedIdentitiesStore);
    const { actor, identityNumber } = get(authenticatedStore);
    const name = selected?.name;
    if (isNullish(name)) {
      throw new Error("Identity is missing a name");
    }

    const passkeyIdentity =
      features.DUMMY_AUTH || nonNullish(canisterConfig.dummy_auth[0]?.[0])
        ? await DiscoverableDummyIdentity.createNew(name)
        : await DiscoverablePasskeyIdentity.createNew(name);
    const credentialId = passkeyIdentity.getCredentialId();
    if (isNullish(credentialId)) {
      throw new Error("Credential ID is missing");
    }
    const uaParser = loadUAParser();
    const alias = await inferPasskeyAlias({
      authenticatorType: passkeyIdentity.getAuthenticatorAttachment(),
      userAgent: navigator.userAgent,
      uaParser,
      aaguid: passkeyIdentity.getAaguid(),
    });
    const authnMethodData = passkeyAuthnMethodData({
      alias,
      pubKey: passkeyIdentity.getPublicKey().toDer(),
      credentialId,
      authenticatorAttachment: passkeyIdentity.getAuthenticatorAttachment(),
      origin: window.location.origin,
    });
    await actor
      .authn_method_add(identityNumber, authnMethodData)
      .then(throwCanisterError);
    return authnMethodData;
  };

  continueWithPasskey = () => {
    this.view = "addPasskey";
  };

  continueOnAnotherDevice = async (): Promise<void> => {
    const { actor, identityNumber } = get(authenticatedStore);
    const registrationId = secureRandomId(REGISTRATION_ID_LENGTH);

    this.newDeviceLink = new URL(
      `/pair#${registrationId}`,
      window.location.origin,
    );
    this.view = "continueOnAnotherDevice";
    this.isRegistrationWindowPassed = false;
    this.isRegistrationWindowOpen = true;

    try {
      // Always exit any ongoing registration mode first
      await actor.authn_method_registration_mode_exit(identityNumber);
      const { expiration } = await actor
        .authn_method_registration_mode_enter(identityNumber, [registrationId])
        .then(throwCanisterError);

      while (!this.isRegistrationWindowPassed) {
        const info = await actor
          .identity_info(identityNumber)
          .then(throwCanisterError);
        // Exit if registration window was manually closed
        if (!this.isRegistrationWindowOpen) {
          break;
        }
        // Show confirmation code view if we got a pending authn method
        if (nonNullish(info.authn_method_registration[0]?.authn_method[0])) {
          this.#waitingForDevice =
            info.authn_method_registration[0]?.authn_method[0];
          this.view = "confirmationCode";
          break;
        }
        // Wait before retrying
        await waitFor(POLL_INTERVAL);
        // Check if we're still in the registration time window
        this.isRegistrationWindowPassed =
          BigInt(Date.now()) * BigInt(1_000_000) > expiration;
      }
    } finally {
      // Close registration window if passed or error was thrown
      this.isRegistrationWindowOpen = false;
      this.newDeviceLink = undefined;
    }
  };

  confirmDevice = async (confirmationCode: string): Promise<void> => {
    const { actor, identityNumber } = get(authenticatedStore);
    await actor
      .authn_method_confirm(identityNumber, confirmationCode)
      .then(throwCanisterError);
    this.view = "continueOnNewDevice";

    if (
      isNullish(this.#waitingForDevice) ||
      !("WebAuthn" in this.#waitingForDevice.authn_method)
    ) {
      throw new Error("Confirmed device not found");
    }
    const credentialId = new Uint8Array(
      this.#waitingForDevice.authn_method.WebAuthn.credential_id,
    );

    // Since `authn_method_confirm` already closes the registration mode,
    // we'll have to poll `authn_identity_info` instead as alternative approach.
    const expiration = new Date(Date.now() + 300000).getTime(); // 5 min
    while (Date.now() < expiration) {
      const { authn_methods } = await anonymousActor
        .identity_authn_info(identityNumber)
        .then(throwCanisterError);
      // Return when temporary key can no longer be found
      if (
        !authn_methods.some(
          (authnMethod) =>
            "WebAuthn" in authnMethod &&
            bufferEqual(
              new Uint8Array(authnMethod.WebAuthn.credential_id),
              credentialId,
            ),
        )
      ) {
        return;
      }
      // Wait before retrying
      await waitFor(POLL_INTERVAL);
    }
    throw new Error("Registration not completed within time window");
  };

  exitRegistrationMode = (): void => {
    this.isRegistrationWindowOpen = false;
  };
}
