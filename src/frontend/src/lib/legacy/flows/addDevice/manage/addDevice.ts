import type {
  IdentityAnchorInfo,
  Timestamp,
} from "$lib/generated/internet_identity_types";
import { displayError } from "$lib/templates/displayError";
import { withLoader } from "$lib/templates/loader";
import { tentativeDeviceStepper } from "$lib/legacy/flows/addDevice/stepper";
import { AuthenticatedConnection } from "$lib/utils/iiConnection";
import { isNullish } from "@dfinity/utils";
import { addDeviceSuccess } from "../addDeviceSuccess";
import { addCurrentDevice } from "./addCurrentDevice";
import { pollForTentativeDevice } from "./pollForTentativeDevice";
import { verifyTentativeDevice } from "./verifyTentativeDevice";

// The flow for adding a remote (i.e. other browser, non-hardware) device
export const addDevice = async ({
  userNumber,
  connection,
  origin,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  origin: string;
}): Promise<void> => {
  // Enter registration mode and get info about the anchor
  const [timestamp, anchorInfo]: [Timestamp, IdentityAnchorInfo] =
    await withLoader(() =>
      Promise.all([
        connection.enterDeviceRegistrationMode(),
        connection.getAnchorInfo(),
      ]),
    );

  let tentativeDevice = anchorInfo.device_registration[0]?.tentative_device[0];
  if (isNullish(tentativeDevice)) {
    // If no device was tentatively added yet, poll until one is added
    const result = await pollForTentativeDevice(
      userNumber,
      connection,
      timestamp,
      origin,
    );

    if (result === "timeout") {
      // On timeout, notify the user and return back to the caller
      await displayError({
        title: "Timeout Reached",
        message:
          'The timeout has been reached. For security reasons the "add device" process has been aborted.',
        primaryButton: "Ok",
      });
      return;
    } else if (result === "use-fido") {
      // If the user wants to add a FIDO device then we can (should) exit registration mode
      // (only used for adding extra browsers)
      await withLoader(() => connection.exitDeviceRegistrationMode());
      await addCurrentDevice(
        userNumber,
        connection,
        anchorInfo.devices,
        origin,
      );
      return;
    } else if (result === "canceled") {
      // If the user canceled, disable registration mode and return
      await withLoader(() => connection.exitDeviceRegistrationMode());
      return;
    }

    tentativeDevice = result;
  }

  const { alias } = tentativeDevice;

  const result = await verifyTentativeDevice({
    connection,
    alias,
    endTimestamp: timestamp,
  });

  if (result === "verified") {
    await addDeviceSuccess({
      userNumber,
      deviceAlias: alias,
      stepper: tentativeDeviceStepper({ step: "success" }),
    });
  }
};
