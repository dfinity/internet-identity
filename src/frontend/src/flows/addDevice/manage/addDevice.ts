import {
  IdentityAnchorInfo,
  Timestamp,
} from "$generated/internet_identity_types";
import { displayError } from "$src/components/displayError";
import { withLoader } from "$src/components/loader";
import { tentativeDeviceStepper } from "$src/flows/addDevice/stepper";
import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { isNullish } from "@dfinity/utils";
import { addDeviceSuccess } from "../addDeviceSuccess";
import { addFIDODevice } from "./addFIDODevice";
import { pollForTentativeDevice } from "./pollForTentativeDevice";
import { verifyTentativeDevice } from "./verifyTentativeDevice";

// The flow for adding a remote (i.e. other browser, non-hardware) device
export const addDevice = async ({
  userNumber,
  connection,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
}): Promise<void> => {
  // Enter registration mode and get info about the anchor
  const [timestamp, anchorInfo]: [Timestamp, IdentityAnchorInfo] =
    await withLoader(() =>
      Promise.all([
        connection.enterDeviceRegistrationMode(),
        connection.getAnchorInfo(),
      ])
    );

  let tentativeDevice = anchorInfo.device_registration[0]?.tentative_device[0];
  if (isNullish(tentativeDevice)) {
    // If no device was tentatively added yet, poll until one is added
    const result = await pollForTentativeDevice(
      userNumber,
      connection,
      timestamp
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
      await addFIDODevice(userNumber, connection, anchorInfo.devices);
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
