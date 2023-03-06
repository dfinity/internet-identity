import { html, render } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { AsyncCountdown } from "../../../utils/countdown";
import { AuthenticatedConnection } from "../../../utils/iiConnection";
import { delayMillis } from "../../../utils/utils";
import {
  DeviceData,
  Timestamp,
} from "../../../../generated/internet_identity_types";
import { mainWindow } from "../../../components/mainWindow";
import { LEGACY_II_URL } from "../../../config";

const pollForTentativeDeviceTemplate = ({
  userNumber,
  cancel,
  remaining,
}: {
  userNumber: bigint;
  cancel: () => void;
  remaining: AsyncIterable<string>;
}) => {
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
      Time remaining:
      <span id="timer" class="t-strong">${asyncReplace(remaining)}</span>
    </p>
    <button
      @click=${() => cancel()}
      id="cancelAddRemoteDevice"
      class="c-button c-button--secondary"
    >
      Cancel
    </button>
  `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const pollForTentativeDevicePage = (
  props: Parameters<typeof pollForTentativeDeviceTemplate>[0],
  container?: HTMLElement
): void => {
  const contain =
    container ?? (document.getElementById("pageContent") as HTMLElement);
  render(pollForTentativeDeviceTemplate(props), contain);
};

/**
 * Polls for a tentative device to be added and shows instructions on how to continue the device registration process on the new device.
 * @param userNumber anchor of the authenticated user
 * @param connection authenticated II connection
 */
export const pollForTentativeDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  endTimestamp: Timestamp
): Promise<DeviceData | "timeout" | "canceled"> => {
  const countdown = AsyncCountdown.fromNanos(endTimestamp);

  // Show the page with the option to cancel
  const page = new Promise<"canceled">((resolve) =>
    pollForTentativeDevicePage({
      userNumber,
      cancel: () => {
        countdown.stop();
        resolve("canceled");
      },
      remaining: countdown.remainingFormattedAsync(),
    })
  );

  // Poll repeatedly
  const polling = poll(userNumber, connection, () => countdown.hasStopped());

  return await Promise.race([page, polling]);
};

const poll = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  shouldStop: () => boolean
): Promise<DeviceData | "timeout"> => {
  for (;;) {
    if (shouldStop()) {
      return "timeout";
    }

    const anchorInfo = await connection.getAnchorInfo();
    const tentativeDevice =
      anchorInfo.device_registration[0]?.tentative_device[0];
    if (tentativeDevice !== undefined) {
      return tentativeDevice;
    }

    // Debounce a little; in practice won't be noticed by users but
    // will avoid hot looping in case the op becomes near instantaneous.
    await delayMillis(100);
  }
};
