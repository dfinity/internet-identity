import { isNullish } from "@dfinity/utils";
import {
  IdentityAnchorInfo,
  Timestamp,
} from "../../../../generated/internet_identity_types";
import { displayError } from "../../../components/displayError";
import { withLoader } from "../../../components/loader";
import { AuthenticatedConnection } from "../../../utils/iiConnection";
import { renderAddDeviceSuccess } from "./addDeviceSuccess";
import { pollForTentativeDevice } from "./pollForTentativeDevice";
import { verifyTentativeDevice } from "./verifyTentativeDevice";

// The flow for adding a remote (i.e. other browser, non-hardware) device
export const addRemoteDevice = async ({
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
    } else if (result === "canceled") {
      // If the user canceled, disable registration mode and return
      await withLoader(() => connection.exitDeviceRegistrationMode());
      return;
    }

    tentativeDevice = result;
  }

  const { alias } = tentativeDevice;

  await verifyTentativeDevice({
    connection,
    alias,
    endTimestamp: timestamp,
  });

  await renderAddDeviceSuccess({ deviceAlias: alias });
};
