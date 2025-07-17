import { anonymousActor, canisterConfig } from "$lib/globals";
import { isCanisterError, throwCanisterError, waitFor } from "$lib/utils/utils";
import { passkeyAuthnMethodData } from "$lib/utils/authnMethodData";
import { authenticateWithSession } from "$lib/utils/authentication";
import { sessionStore } from "$lib/stores/session.store";
import { get } from "svelte/store";
import { bufferEqual } from "$lib/utils/iiConnection";
import {
  authenticatedStore,
  authenticationStore,
} from "$lib/stores/authentication.store";
import { features } from "$lib/legacy/features";
import { isNullish, nonNullish } from "@dfinity/utils";
import { DiscoverableDummyIdentity } from "$lib/utils/discoverableDummyIdentity";
import { DiscoverablePasskeyIdentity } from "$lib/utils/discoverablePasskeyIdentity";
import { inferPasskeyAlias, loadUAParser } from "$lib/legacy/flows/register";
import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";
import { AuthnMethodRegisterError } from "$lib/generated/internet_identity_types";

const POLL_INTERVAL = 3000;

export class RegisterAccessMethodFlow {
  view = $state<"confirmDevice" | "confirmSignIn">("confirmDevice");
  confirmationCode = $state<string>();
  isUnableToComplete = $state(false);
  identityName = $state<string>();

  registerTempKey = async (registrationId: string): Promise<void> => {
    const identityNumber = (
      await anonymousActor.lookup_by_registration_mode_id(registrationId)
    )[0];
    if (isNullish(identityNumber)) {
      this.isUnableToComplete = true;
      return;
    }
    const session = get(sessionStore);
    const credentialId = crypto.getRandomValues(new Uint8Array(32));
    const authnMethodData = passkeyAuthnMethodData({
      alias: "temporary-key",
      pubKey: session.identity.getPublicKey().toDer(),
      credentialId,
      origin: window.location.origin,
    });
    let expiration: bigint;
    try {
      const confirmation = await anonymousActor
        .authn_method_register(identityNumber, authnMethodData)
        .then(throwCanisterError);
      expiration = confirmation.expiration;
      this.confirmationCode = confirmation.confirmation_code;
    } catch (error) {
      if (isCanisterError<AuthnMethodRegisterError>(error)) {
        switch (error.type) {
          case "RegistrationModeOff":
          case "RegistrationAlreadyInProgress":
            this.isUnableToComplete = true;
            return;
        }
      }
      throw error;
    }

    while (!this.isUnableToComplete) {
      const { authn_methods } = await anonymousActor
        .identity_authn_info(identityNumber)
        .then(throwCanisterError);
      // Show confirm sign-in view if session key has been registered
      if (
        authn_methods.some(
          (authnMethod) =>
            "WebAuthn" in authnMethod &&
            bufferEqual(
              new Uint8Array(authnMethod.WebAuthn.credential_id),
              credentialId,
            ),
        )
      ) {
        const identity = await authenticateWithSession({ session });
        authenticationStore.set({
          identity,
          identityNumber,
        });
        const { name } = await get(authenticatedStore)
          .actor.identity_info(identityNumber)
          .then(throwCanisterError);
        this.identityName = name[0];
        this.view = "confirmSignIn";
        break;
      }
      // Wait before retrying
      await waitFor(POLL_INTERVAL);
      // Check if we're still in the confirmation time window
      this.isUnableToComplete =
        BigInt(Date.now()) * BigInt(1_000_000) > expiration;
    }
  };

  createPasskey = async (): Promise<void> => {
    const session = get(sessionStore);
    const { actor, identityNumber } = get(authenticatedStore);
    const name = this.identityName ?? identityNumber.toString(10);
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
      .authn_method_replace(
        identityNumber,
        new Uint8Array(session.identity.getPublicKey().toDer()),
        authnMethodData,
      )
      .then(throwCanisterError);
    lastUsedIdentitiesStore.addLastUsedIdentity({
      identityNumber,
      name,
      authMethod: {
        passkey: {
          credentialId: new Uint8Array(credentialId),
        },
      },
    });
  };
}
