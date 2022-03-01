import {html, render} from "lit-html";
import {IIConnection} from "../../utils/iiConnection";
import {renderManage} from "../manage";
import {withLoader} from "../../components/loader";
import {verifyDevice} from "./verifyTentativeDevice";
import {Countdown, formatRemainingTime, setupCountdown} from "../../utils/countdown";

const pageContent = (endTimestamp: bigint) => html`
  <div class="container">
    <h1>Add New Remote Device</h1>
    <p>
      Device registration process started. Please follow these steps to add your
      new device:
    </p>
    <ol>
      <li>Open <b>https://identity.ic0.app</b> on your remote machine</li>
      <li>Chose <b>Already have an anchor but using a new device?</b></li>
      <li>Follow the instructions displayed on your remote machine</li>
    </ol>
    <p>Time remaining: <span id="timer">${formatRemainingTime(endTimestamp)}</span></p>
    <button id="cancelAddRemoteDevice" class="linkStyle">Cancel</button>
  </div>
`;

export const pollForTentativeDevice = async (
  userNumber: bigint,
  connection: IIConnection
): Promise<void> => {
  await withLoader(async () => {
    const timestamp = await connection.enableDeviceRegistrationMode(userNumber);
    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(timestamp), container);
    init(userNumber, timestamp, connection);
  });
};

const startPolling = (
  connection: IIConnection,
  userNumber: bigint,
  timerUpdate: Countdown,
  endTimestamp: bigint): number => {
  const pollingHandle = window.setInterval(async () => {
    const userInfo = await connection.lookupAnchorInfo(userNumber);
    if (userInfo.tentative_device.length === 1) {
      const tentative_device = userInfo.tentative_device[0];
      window.clearInterval(pollingHandle);
      timerUpdate.stop();
      await verifyDevice(userNumber, tentative_device, endTimestamp, connection);
    }
  }, 2000);
  return pollingHandle;
};

const init = (
  userNumber: bigint,
  endTimestamp: bigint,
  connection: IIConnection
) => {
  const countdown = setupCountdown(endTimestamp, () => renderManage(userNumber, connection));
  const pollingHandle = startPolling(connection, userNumber, countdown, endTimestamp);

  const cancelButton = document.getElementById(
    "cancelAddRemoteDevice"
  ) as HTMLButtonElement;
  cancelButton.onclick = async () => {
    window.clearInterval(pollingHandle);
    countdown.stop();
    await withLoader(() =>
      connection.disableDeviceRegistrationMode(userNumber)
    );
    await renderManage(userNumber, connection);
  };
};
