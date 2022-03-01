import { html, render } from "lit-html";
import { IIConnection } from "../../utils/iiConnection";
import { renderManage } from "../manage";
import { withLoader } from "../../components/loader";
import { verifyDevice } from "./verifyTentativeDevice";
import { Countdown } from "../../utils/countdown";
import { displayError } from "../../components/displayError";

const pageContent = (minRemaining: string, secondsRemaining: string) => html`
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
    <p id="timer">Time remaining: ${minRemaining}:${secondsRemaining}</p>
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
    render(pageContent(...calculateTimeRemaining(timestamp)), container);
    init(userNumber, timestamp, connection);
  });
};

const startPolling = (
  connection: IIConnection,
  userNumber: bigint,
  timerUpdate: Countdown
): number => {
  const pollingHandle = window.setInterval(async () => {
    const userInfo = await connection.lookupAnchorInfo(userNumber);
    if (userInfo.tentative_device.length === 1) {
      const tentative_device = userInfo.tentative_device[0];
      window.clearInterval(pollingHandle);
      timerUpdate.stop();
      await verifyDevice(userNumber, tentative_device, connection);
    }
  }, 2000);
  return pollingHandle;
};

const setupCountdown = (
  connection: IIConnection,
  userNumber: bigint,
  timestamp: bigint
): Countdown => {
  const container = document.getElementById("pageContent") as HTMLElement;
  return new Countdown(
    () => render(pageContent(...calculateTimeRemaining(timestamp)), container),
    1000,
    async () => {
      await displayError({
        title: "Timeout reached",
        message:
          "The timeout has been reached. For security reasons, the add device process has been aborted.",
        primaryButton: "Back to manage",
      });
      await renderManage(userNumber, connection);
    },
    timestamp
  );
};

const init = (
  userNumber: bigint,
  timestamp: bigint,
  connection: IIConnection
) => {
  const countdown = setupCountdown(connection, userNumber, timestamp);
  const pollingHandle = startPolling(connection, userNumber, countdown);

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

const calculateTimeRemaining = (
  expirationTimestamp: bigint
): [string, string] => {
  const now = new Date().getTime();
  const diffSeconds =
    (Number(expirationTimestamp / BigInt("1000000")) - now) / 1000;
  return [
    Math.floor(diffSeconds / 60).toString(),
    Math.floor(diffSeconds % 60)
      .toString()
      .padStart(2, "0"),
  ];
};
