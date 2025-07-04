import { DeviceData, UserNumber } from "$lib/generated/internet_identity_types";
import { authenticatedStore } from "$lib/stores/authentication.store";
import { waitFor } from "$lib/utils/utils";
import { nonNullish } from "@dfinity/utils";
import { get } from "svelte/store";

export class StartAddPasskeyFlow {
  view = $state<"show-link" | "authorize" | "success">("show-link");
  #identityNumber: UserNumber;
  #tentativeDevice: DeviceData | undefined;

  constructor(identityNumber: UserNumber) {
    this.#identityNumber = identityNumber;
    this.#tentativeDevice = undefined;
  }

  startAddPasskeyFlow = async () => {
    await this.#enterRegistrationMode();
    this.#tentativeDevice = await this.#pollForTentativeDevice();
    this.view = "authorize";
  };

  verifyDevice = async (code: string) => {
    const actor = await get(authenticatedStore).actor;
    const response = await actor.verify_tentative_device(
      this.#identityNumber,
      code,
    );
    if ("verified" in response) {
      this.view = "success";
    }
  };

  #enterRegistrationMode = async () => {
    const actor = await get(authenticatedStore).actor;
    await actor.enter_device_registration_mode(this.#identityNumber);
  };

  #pollForTentativeDevice = async (): Promise<DeviceData | undefined> => {
    const actor = await get(authenticatedStore).actor;
    // eslint-disable-next-line no-async-promise-executor
    return new Promise<DeviceData | undefined>(async (resolve, reject) => {
      // TODO: Add countdown or timeout
      let counter = 0;
      const maxCount = 1000;
      while (counter < maxCount) {
        const anchorInfo = await actor.get_anchor_info(this.#identityNumber);
        const tentativeDevice =
          anchorInfo.device_registration[0]?.tentative_device[0];
        if (nonNullish(tentativeDevice)) {
          resolve(tentativeDevice);
          return;
        }
        // Debounce a little; in practice won't be noticed by users but
        // will avoid hot looping in case the op becomes near instantaneous.
        await waitFor(100);
        counter++;
        if (counter >= maxCount) {
          reject(Error("Failed to find tentative device")); // or reject with an error
          return;
        }
      }
    });
  };
}
