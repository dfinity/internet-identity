import { html, render } from "lit-html";
import { AuthenticatedConnection } from "../../../utils/iiConnection";
import { withLoader } from "../../../components/loader";
import { verifyTentativeDevice } from "./verifyTentativeDevice";
import { setupCountdown } from "../../../utils/countdown";
import {
  DeviceData,
  IdentityAnchorInfo,
} from "../../../../generated/internet_identity_types";
import { displayError } from "../../../components/displayError";
import { mainWindow } from "../../../components/mainWindow";
import { LEGACY_II_URL } from "../../../config";

const pageContent = (userNumber: bigint) => {
  const pageContentSlot = html`
    <hgroup>
      <h1 class="t-title t-title--main">Add a Trusted Device</h1>
      <p class="t-lead">
        Complete the steps below
        <strong class="t-string"> on the device you want to add: </strong>
      </p>
    </hgroup>
    <ol class="c-list c-list--numbered l-stack">
      <li>
        Open
        <em class="c-tooltip">
          <strong class="t-strong">${LEGACY_II_URL}</strong>
          <span class="c-tooltip__message c-card c-card--tight">
            Open this link on the device you want to add.
          </span>
        </em>
      </li>
      <li>Select <strong class="t-strong">“Add a new device?”</strong></li>
      <li>
        Enter your Identity Anchor:
        <strong class="t-strong">${userNumber}</strong>
      </li>
      <li>Name your new device</li>
    </ol>
    <p class="t-paragraph">
      This page will automatically refresh after completing the above steps.
    </p>
    <p class="t-paragraph">
      Time remaining: <span id="timer" class="t-strong"></span>
    </p>
    <button id="cancelAddRemoteDevice" class="c-button c-button--secondary">
      Cancel
    </button>
  `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

/**
 * Polls for a tentative device to be added and shows instructions on how to continue the device registration process on the new device.
 * @param userNumber anchor of the authenticated user
 * @param connection authenticated II connection
 */
export const pollForTentativeDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  const [timestamp, tentativeDevice] = await withLoader(async () =>
    Promise.all([
      connection.enterDeviceRegistrationMode(),
      getTentativeDevice(await connection.getAnchorInfo()),
    ])
  );

  if (tentativeDevice) {
    // directly show the verification screen if the tentative device already exists
    await verifyTentativeDevice({
      connection,
      alias: tentativeDevice.alias,
      endTimestamp: timestamp,
    });
    return;
  } else {
    renderPollForTentativeDevicePage(userNumber);
    await init(userNumber, connection, timestamp);
  }
};

export const renderPollForTentativeDevicePage = (userNumber: bigint): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
};

const poll = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  shouldStop: () => boolean
): Promise<DeviceData | null> => {
  return connection.getAnchorInfo().then((response) => {
    if (shouldStop()) {
      return null;
    }
    const tentativeDevice = getTentativeDevice(response);
    if (tentativeDevice) {
      return tentativeDevice;
    }
    return poll(userNumber, connection, shouldStop);
  });
};

const init = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  endTimestamp: bigint
): Promise<void> =>
  new Promise((resolve) => {
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
        resolve();
      }
    );

    void poll(userNumber, connection, () => countdown.hasStopped()).then(
      async (device) => {
        if (!countdown.hasStopped() && device) {
          countdown.stop();
          await verifyTentativeDevice({
            connection,
            alias: device.alias,
            endTimestamp,
          });
          resolve();
        }
      }
    );

    const cancelButton = document.getElementById(
      "cancelAddRemoteDevice"
    ) as HTMLButtonElement;
    cancelButton.onclick = async () => {
      countdown.stop();
      await withLoader(() => connection.exitDeviceRegistrationMode());
      resolve();
    };
  });

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
