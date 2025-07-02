import { DeviceData, UserNumber } from "$lib/generated/internet_identity_types";
import { sessionStore } from "$lib/stores/session.store";
import { get } from "svelte/store";

export class AddPasskeyFlow {
  view = $state<"loading" | "show-code" | "add-device" | "success">("loading");
  verificationCode: string | undefined;
  #identityNumber: UserNumber;
  #tentativeDevice: DeviceData;

  constructor(identityNumber: UserNumber) {
    this.#identityNumber = identityNumber;
    this.#tentativeDevice = {
      alias: "temporary-key",
      metadata: [],
      origin: [window.location.origin],
      protection: { unprotected: null },
      pubkey: new Uint8Array(),
      purpose: { authentication: null },
      key_type: { unknown: null },
      credential_id: [],
    };
    this.verificationCode = undefined;
  }

  addTemporaryKey = async () => {
    console.log("adding temporary key");
    const response = await get(sessionStore).actor.add_tentative_device(
      this.#identityNumber,
      this.#tentativeDevice,
    );
    console.log(response);
    if ("added_tentatively" in response) {
      this.verificationCode = response.added_tentatively.verification_code;
      this.view = "show-code";
    }
  };

  addPasskey = async () => {
    // TODO: Use temporary key to add a new passkey
  };
}
