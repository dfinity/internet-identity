import { html, render } from "lit-html";
import { IIConnection } from "../../../utils/iiConnection";
import { renderManage } from "../../manage";
import { withLoader } from "../../../components/loader";
import { verifyDevice } from "./verifyTentativeDevice";
import { Countdown, setupCountdown } from "../../../utils/countdown";
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

export const pollForTentativeDevice = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  await withLoader(async () => {
    const [timestamp, userInfo] = await Promise.all([
      connection.enterDeviceRegistrationMode(userNumber),
      connection.getAnchorInfo(userNumber),
    ]);
    const tentative_device = getTentativeDevice(userInfo);
    if (tentative_device) {
      // directly show the verification screen if the tentative device already exists
      await verifyDevice(userNumber, tentative_device, timestamp, connection);
    } else {
      const container = document.getElementById("pageContent") as HTMLElement;
      render(pageContent(userNumber), container);
      init(userNumber, timestamp, connection);
    }
  });
};

const startPolling = (
  connection: IIConnection,
  userNumber: bigint,
  timerUpdate: Countdown,
  endTimestamp: bigint
): number => {
  const pollingHandle = window.setInterval(async () => {
    const userInfo = await connection.getAnchorInfo(userNumber);
    const tentative_device = getTentativeDevice(userInfo);
    if (tentative_device) {
      window.clearInterval(pollingHandle);
      timerUpdate.stop();
      await verifyDevice(
        userNumber,
        tentative_device,
        endTimestamp,
        connection
      );
    }
  }, 3000);
  return pollingHandle;
};

const init = (
  userNumber: bigint,
  endTimestamp: bigint,
  connection: IIConnection
) => {
  const countdown = setupCountdown(
    endTimestamp,
    document.getElementById("timer") as HTMLElement
  );
  const pollingHandle = startPolling(
    connection,
    userNumber,
    countdown,
    endTimestamp
  );
  countdown.start(async () => {
    window.clearInterval(pollingHandle);
    await displayError({
      title: "Timeout Reached",
      message:
        'The timeout has been reached. For security reasons the "add device" process has been aborted.',
      primaryButton: "Ok",
    });
    await renderManage(userNumber, connection);
  });

  const cancelButton = document.getElementById(
    "cancelAddRemoteDevice"
  ) as HTMLButtonElement;
  cancelButton.onclick = async () => {
    window.clearInterval(pollingHandle);
    countdown.stop();
    await withLoader(() => connection.exitDeviceRegistrationMode(userNumber));
    await renderManage(userNumber, connection);
  };
};

const getTentativeDevice = (
  userInfo: IdentityAnchorInfo
): DeviceData | undefined => {
  if (
    userInfo.device_registration.length === 1 &&
    userInfo.device_registration[0].tentative_device.length === 1
  ) {
    return userInfo.device_registration[0].tentative_device[0];
  }
  return undefined;
};
