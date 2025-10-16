import { get } from "svelte/store";
import { isNullish, nonNullish } from "@dfinity/utils";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { secureRandomId, throwCanisterError, waitFor } from "$lib/utils/utils";

const POLL_INTERVAL = 3000; // Should be frequent enough

export class ConfirmAccessMethodFlow {
  #view = $state<
    "continueOnNewDevice" | "enterConfirmationCode" | "finishOnNewDevice"
  >("continueOnNewDevice");
  #newDeviceLink = $state<URL>();
  #sessionExpiration = $state<bigint>();

  get view() {
    return this.#view;
  }

  get newDeviceLink() {
    return this.#newDeviceLink;
  }

  enterRegistrationMode = async (existingRegistrationId?: string) => {
    const { actor, identityNumber } = get(authenticatedStore);
    const registrationId = existingRegistrationId ?? secureRandomId(5);

    if (isNullish(existingRegistrationId)) {
      this.#newDeviceLink = new URL(
        `/pair#${registrationId}`,
        window.location.origin,
      );
    }
    this.#view = "continueOnNewDevice";

    // Always exit any ongoing registration mode first
    await this.exitRegistrationMode();
    const { expiration } = await actor
      .authn_method_registration_mode_enter(identityNumber, [registrationId])
      .then(throwCanisterError);

    try {
      while (BigInt(Date.now()) * BigInt(1_000_000) < expiration) {
        const info = await actor
          .identity_info(identityNumber)
          .then(throwCanisterError);
        // Exit if registration window was closed
        if (isNullish(info.authn_method_registration[0])) {
          break;
        }
        // Show confirmation code view if we got a pending session
        if (nonNullish(info.authn_method_registration[0]?.session[0])) {
          this.#sessionExpiration =
            info.authn_method_registration[0].expiration;
          this.#view = "enterConfirmationCode";
          break;
        }
        // Wait before retrying
        await waitFor(POLL_INTERVAL);
      }
    } catch (error) {
      // Close registration window if error was thrown
      await this.exitRegistrationMode();
      throw error;
    }
  };

  confirmDevice = async (confirmationCode: string): Promise<void> => {
    const { actor, identityNumber } = get(authenticatedStore);
    await actor
      .authn_method_confirm(identityNumber, confirmationCode)
      .then(throwCanisterError);
    this.#view = "finishOnNewDevice";

    if (isNullish(this.#sessionExpiration)) {
      throw new Error("Session expiration is missing");
    }
    while (BigInt(Date.now()) * BigInt(1_000_000) < this.#sessionExpiration) {
      const info = await actor
        .identity_info(identityNumber)
        .then(throwCanisterError);
      // Return when registration window is closed
      if (isNullish(info.authn_method_registration[0])) {
        return;
      }
      // Wait before retrying
      await waitFor(POLL_INTERVAL);
    }
    throw new Error("Registration not completed within time window");
  };

  exitRegistrationMode = async (): Promise<void> => {
    const { actor, identityNumber } = get(authenticatedStore);
    await actor.authn_method_registration_mode_exit(identityNumber, []);
  };
}
