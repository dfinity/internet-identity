import { html, render } from "lit-html";
import { IIConnection } from "../../../utils/iiConnection";
import { renderManage } from "../../manage";
import { withLoader } from "../../../components/loader";
import { verifyDevice } from "./verifyTentativeDevice";
import { setupCountdown } from "../../../utils/countdown";
import {
  DeviceData,
  IdentityAnchorInfo,
} from "../../../../generated/internet_identity_types";
import { displayError } from "../../../components/displayError";

const pageContent = (userNumber: bigint) => html`
  <div class="container">
    <h1>Add New Remote Device</h1>
    <p>
      Device registration mode enabled for Identity Anchor
      <strong>${userNumber}</strong>. Please follow these steps to add your new
      device:
    </p>
    <ol class="instruction-steps">
      <li>
        On your <em>new</em> device:<br />
        Open <strong>https://identity.ic0.app</strong>
      </li>
      <li>
        On your <em>new</em> device:<br />
        Chose
        <b>Already have an anchor but using a new device?</b>
      </li>
      <li>
        On your <em>new</em> device:<br />
        Enter your Identity Anchor
        <strong>${userNumber}</strong>
      </li>
      <li>
        On your <em>new</em> device:<br />
        Choose an alias for your new device
      </li>
    </ol>
    <p>
      This page will automatically refresh after completing the above steps.
    </p>
    <p>Time remaining: <span id="timer"></span></p>
    <button id="cancelAddRemoteDevice" class="linkStyle">Cancel</button>
  </div>
`;

/**
 * Polls for a tentative device to be added and shows instructions on how to continue the device registration process on the new device.
 * @param userNumber anchor of the authenticated user
 * @param connection authenticated II connection
 */
export const pollForTentativeDevice = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  await withLoader(async () => {
    const [timestamp, userInfo] = await Promise.all([
      connection.enterDeviceRegistrationMode(userNumber),
      connection.getAnchorInfo(userNumber),
    ]);
    const tentativeDevice = getTentativeDevice(userInfo);
    if (tentativeDevice) {
      // directly show the verification screen if the tentative device already exists
      await verifyDevice(userNumber, tentativeDevice, timestamp, connection);
    } else {
      const container = document.getElementById("pageContent") as HTMLElement;
      render(pageContent(userNumber), container);
      init(userNumber, timestamp, connection);
    }
  });
};

const poll = (
  connection: IIConnection,
  userNumber: bigint,
  shouldStop: () => boolean
): Promise<DeviceData | null> => {
  return connection.getAnchorInfo(userNumber).then((response) => {
    if (shouldStop()) {
      return null;
    }
    const tentativeDevice = getTentativeDevice(response);
    if (tentativeDevice) {
      return tentativeDevice;
    }
    return poll(connection, userNumber, shouldStop);
  });
};

const init = (
  userNumber: bigint,
  endTimestamp: bigint,
  connection: IIConnection
) => {
  const countdown = setupCountdown(
    endTimestamp,
    document.getElementById("timer") as HTMLElement,
    async () => {
      await displayError({
        title: "Timeout Reached",
        message:
          'The timeout has been reached. For security reasons the "add device" process has been aborted.',
        primaryButton: "Ok",
      });
      await renderManage(userNumber, connection);
    }
  );

  poll(connection, userNumber, () => countdown.hasStopped()).then(
    async (device) => {
      if (!countdown.hasStopped() && device) {
        countdown.stop();
        await verifyDevice(userNumber, device, endTimestamp, connection);
      }
    }
  );

  const cancelButton = document.getElementById(
    "cancelAddRemoteDevice"
  ) as HTMLButtonElement;
  cancelButton.onclick = async () => {
    countdown.stop();
    await withLoader(() => connection.exitDeviceRegistrationMode(userNumber));
    await renderManage(userNumber, connection);
  };
};

const getTentativeDevice = (
  userInfo: IdentityAnchorInfo
): DeviceData | null => {
  if (
    userInfo.device_registration.length === 1 &&
    userInfo.device_registration[0].tentative_device.length === 1
  ) {
    return userInfo.device_registration[0].tentative_device[0];
  }
  return null;
};
