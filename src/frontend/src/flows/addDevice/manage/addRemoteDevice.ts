import { AuthenticatedConnection } from "../../../utils/iiConnection";
import { pollForTentativeDevice } from "./pollForTentativeDevice";
import { displayError } from "../../../components/displayError";
import { verifyTentativeDevice } from "./verifyTentativeDevice";
import { withLoader } from "../../../components/loader";

// The flow for adding a remote (i.e. other browser, non-hardware) device
export const addRemoteDevice = async ({
  userNumber,
  connection,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
}): Promise<void> => {
  // Enter registration mode and get info about the anchor
  const [timestamp, anchorInfo] = await withLoader(() =>
    Promise.all([
      connection.enterDeviceRegistrationMode(),
      connection.getAnchorInfo(),
    ])
  );

  let tentativeDevice = anchorInfo.device_registration[0]?.tentative_device[0];
  if (tentativeDevice === undefined) {
    // If no device was tentatively added yet, poll until one is added
    const result = await pollForTentativeDevice(
      userNumber,
      connection,
      timestamp
    );

    if (result === "timeout") {
      // On timeout, notify the user
      await displayError({
        title: "Timeout Reached",
        message:
          'The timeout has been reached. For security reasons the "add device" process has been aborted.',
        primaryButton: "Ok",
      });
      return;
    } else if (result === "canceled") {
      // If the user canceled, disable registration mode and return
      await connection.exitDeviceRegistrationMode();
      return;
    }

    tentativeDevice = result;
  }

  await verifyTentativeDevice({
    connection,
    alias: tentativeDevice.alias,
    endTimestamp: timestamp,
  });
};
