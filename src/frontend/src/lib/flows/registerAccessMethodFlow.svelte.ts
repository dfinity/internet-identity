import { anonymousActor, canisterConfig } from "$lib/globals";
import { secureRandomId, throwCanisterError, waitFor } from "$lib/utils/utils";
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

const POLL_INTERVAL = 3000; // Should be frequent enough

export class RegisterAccessMethodFlow {
  #view = $state<
    "continueFromExistingDevice" | "confirmDevice" | "confirmSignIn"
  >("continueFromExistingDevice");
  #confirmationCode = $state<string>();
  #identityName = $state<string>();
  #existingDeviceLink = $state<URL>();

  get view() {
    return this.#view;
  }

  get confirmationCode() {
    return this.#confirmationCode;
  }

  get identityName() {
    return this.#identityName;
  }

  get existingDeviceLink() {
    return this.#existingDeviceLink;
  }

  waitForExistingDevice = async (existingRegistrationId?: string) => {
    const registrationId = existingRegistrationId ?? secureRandomId(5);
    if (isNullish(existingRegistrationId)) {
      this.#existingDeviceLink = new URL(
        `/activate#${registrationId}`,
        window.location.origin,
      );
    }
    this.#view = "continueFromExistingDevice";

    const expiration = new Date(Date.now() + 300000).getTime(); // 5 min
    while (Date.now() < expiration) {
      const identityNumber = (
        await anonymousActor.lookup_by_registration_mode_id(registrationId)
      )[0];
      if (nonNullish(identityNumber)) {
        return this.#registerTempKey(identityNumber);
      }
      // Wait before retrying
      await waitFor(POLL_INTERVAL);
    }
    throw new Error("Registration not completed within time window");
  };

  #registerTempKey = async (identityNumber: bigint): Promise<void> => {
    const session = get(sessionStore);
    const credentialId = crypto.getRandomValues(new Uint8Array(32));
    const authnMethodData = passkeyAuthnMethodData({
      alias: "temporary-key",
      pubKey: session.identity.getPublicKey().toDer(),
      credentialId,
      origin: window.location.origin,
    });
    const confirmation = await anonymousActor
      .authn_method_register(identityNumber, authnMethodData)
      .then(throwCanisterError);
    const expiration = confirmation.expiration;
    this.#confirmationCode = confirmation.confirmation_code;
    this.#view = "confirmDevice";

    while (BigInt(Date.now()) * BigInt(1_000_000) < expiration) {
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
        this.#identityName = name[0];
        this.#view = "confirmSignIn";
        return;
      }
      // Wait before retrying
      await waitFor(POLL_INTERVAL);
    }
    throw new Error("Registration not completed within time window");
  };

  createPasskey = async (): Promise<bigint> => {
    const session = get(sessionStore);
    const { actor, identityNumber } = get(authenticatedStore);
    const name = this.#identityName ?? identityNumber.toString(10);
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
    return identityNumber;
  };
}
