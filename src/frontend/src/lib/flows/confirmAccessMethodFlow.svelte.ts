import { anonymousActor } from "$lib/globals";
import { get } from "svelte/store";
import { isNullish, nonNullish } from "@dfinity/utils";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { secureRandomId, throwCanisterError, waitFor } from "$lib/utils/utils";
import type { AuthnMethodData } from "$lib/generated/internet_identity_types";
import { bufferEqual } from "$lib/utils/iiConnection";

const POLL_INTERVAL = 3000; // Should be frequent enough

// abstract class View<T extends Flow<T>> {
//   protected readonly flow: Flow<T>;
//
//   constructor(flow: Flow<T>) {
//     this.#flow = flow;
//   }
// }
//
// interface Flow<T extends Flow<T>> {
//   view: View<T>;
// }

export class ConfirmAccessMethodFlow2 {
  #view = $state<
    | InstanceType<typeof ConfirmAccessMethodFlow2.WaitingForNewDevice>
    | InstanceType<typeof ConfirmAccessMethodFlow2.ContinueOnNewDevice>
    | InstanceType<typeof ConfirmAccessMethodFlow2.EnterConfirmationCodeView>
    | InstanceType<typeof ConfirmAccessMethodFlow2.FinishOnNewDevice>
  >(new ConfirmAccessMethodFlow2.ContinueOnNewDevice(this));
  #waitingForDevice?: AuthnMethodData;
  #newDeviceLink = $state<URL>();

  constructor(existingRegistrationId?: string) {}

  get view() {
    return this.#view;
  }

  static ContinueOnNewDevice = class {
    readonly #flow: ConfirmAccessMethodFlow2;

    constructor(flow: ConfirmAccessMethodFlow2) {
      this.#flow = flow;
    }

    get newDeviceLink() {
      return this.#flow.#newDeviceLink;
    }
  };

  static WaitingForNewDevice = class {
    readonly #flow: ConfirmAccessMethodFlow2;

    constructor(flow: ConfirmAccessMethodFlow2) {
      this.#flow = flow;
    }

    get newDeviceLink() {
      return this.#flow.#newDeviceLink;
    }
  };

  static EnterConfirmationCodeView = class {
    readonly #flow: ConfirmAccessMethodFlow2;

    constructor(flow: ConfirmAccessMethodFlow2) {
      this.#flow = flow;
    }

    confirmDevice = async (confirmationCode: string): Promise<void> => {
      const { actor, identityNumber } = get(authenticatedStore);
      await actor
        .authn_method_confirm(identityNumber, confirmationCode)
        .then(throwCanisterError);
      this.#flow.#view = new ConfirmAccessMethodFlow2.FinishOnNewDevice(
        this.#flow,
      );

      if (
        isNullish(this.#flow.#waitingForDevice) ||
        !("WebAuthn" in this.#flow.#waitingForDevice.authn_method)
      ) {
        throw new Error("Confirmed device not found");
      }
      const credentialId = new Uint8Array(
        this.#flow.#waitingForDevice.authn_method.WebAuthn.credential_id,
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
  };

  static FinishOnNewDevice = class {
    readonly #flow: ConfirmAccessMethodFlow2;

    constructor(flow: ConfirmAccessMethodFlow2) {
      this.#flow = flow;
    }

    get newDeviceLink() {
      return this.#flow.#newDeviceLink;
    }
  };

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
        // Show confirmation code view if we got a pending authn method
        if (nonNullish(info.authn_method_registration[0]?.authn_method[0])) {
          this.#waitingForDevice =
            info.authn_method_registration[0]?.authn_method[0];
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

  exitRegistrationMode = async (): Promise<void> => {
    const { actor, identityNumber } = get(authenticatedStore);
    await actor.authn_method_registration_mode_exit(identityNumber);
  };
}

const x = new ConfirmAccessMethodFlow2();
if (x.view.name === "continueOnNewDeviceView") {
  const y = x.view.newDeviceLink;
}

export class ConfirmAccessMethodFlow {
  #view = $state<
    "continueOnNewDevice" | "enterConfirmationCode" | "finishOnNewDevice"
  >("continueOnNewDevice");
  #waitingForDevice?: AuthnMethodData;
  #newDeviceLink = $state<URL>();

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
        // Show confirmation code view if we got a pending authn method
        if (nonNullish(info.authn_method_registration[0]?.authn_method[0])) {
          this.#waitingForDevice =
            info.authn_method_registration[0]?.authn_method[0];
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

  exitRegistrationMode = async (): Promise<void> => {
    const { actor, identityNumber } = get(authenticatedStore);
    await actor.authn_method_registration_mode_exit(identityNumber);
  };
}
