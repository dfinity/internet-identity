import { html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import type QrCreator from "qr-creator"; // XXX: import to only import the _type_ to avoid pulling in the whole module (module itself is used as a dynamic import)
import {
  DeviceData,
  Timestamp,
} from "../../../../generated/internet_identity_types";
import { checkmarkIcon, copyIcon } from "../../../components/icons";
import { mainWindow } from "../../../components/mainWindow";
import { toast } from "../../../components/toast";
import { addDeviceLink } from "../../../utils/addDeviceLink";
import { AsyncCountdown } from "../../../utils/countdown";
import { AuthenticatedConnection } from "../../../utils/iiConnection";
import { mount, renderPage, withRef } from "../../../utils/lit-html";
import { delayMillis } from "../../../utils/utils";

const pollForTentativeDeviceTemplate = ({
  userNumber,
  cancel,
  remaining,
  origin,
}: {
  userNumber: bigint;
  cancel: () => void;
  remaining: AsyncIterable<string>;
  origin: string;
}) => {
  const link = addDeviceLink({ userNumber, origin });
  const copyLink_ = () => navigator.clipboard.writeText(link);

  const linkCopyElement: Ref<HTMLElement> = createRef();
  // Copy the link and give visual feedback on success
  const copyLink = async () => {
    try {
      await copyLink_();
      withRef(linkCopyElement, (linkCopyElement) => {
        linkCopyElement.classList.add("is-copied");
      });
    } catch (e: unknown) {
      toast.error("Could not copy link to clipboard");
      console.error("Could not copy link to clipboard", e);
    }
  };

  const pageContentSlot = html`
    <hgroup>
      <h2 class="t-paragraph">Identity Anchor ${userNumber}</h2>
      <h1 class="t-title t-title--main l-stack--none">Add Trusted Device</h1>
      <p class="t-lead">Follow these steps on your new device</p>
    </hgroup>
    <div
      class="t-centered c-qrcode l-stack"
      ${mount((container) =>
        container instanceof HTMLElement
          ? displayQR({ link, container })
          : undefined
      )}
    ></div>
    <p data-role="add-device-link" class="t-paragraph l-stack t-wrap">
      ${link}
    </p>
    <!-- this is an absolute hack: position relative is needed for c-button__icon to be anchored, and
        padding-bottom is needed otherwise the button overlaps the text -->
    <div class="t-centered" style="position: relative; padding-bottom: 2em;">
      <i
        ${ref(linkCopyElement)}
        @click=${() => copyLink()}
        aria-label="Copy to clipboard"
        title="Copy to clipboard"
        tabindex="0"
        id="seedCopy"
        data-action="copy-link"
        class="c-button__icon"
      >
        <span>Copy</span>
        ${copyIcon} ${checkmarkIcon}
      </i>
    </div>

    <ol class="c-list c-list--numbered l-stack">
      <li>Scan the QR code with your device’s camera or visit the link.</li>
      <li>Sign in using your device.</li>
    </ol>

    <button
      @click=${() => cancel()}
      id="cancelAddRemoteDevice"
      class="c-button c-button--secondary"
    >
      Cancel
    </button>
    <p class="t-paragraph">
      Time remaining:
      <span id="timer" class="t-strong">${asyncReplace(remaining)}</span>
    </p>
  `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const pollForTentativeDevicePage = renderPage(
  pollForTentativeDeviceTemplate
);

/**
 * Polls for a tentative device to be added and shows instructions on how to continue the device registration process on the new device.
 * @param userNumber anchor of the authenticated user
 * @param connection authenticated II connection
 */
export const pollForTentativeDevice = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  endTimestamp: Timestamp
): Promise<DeviceData | "timeout" | "canceled"> => {
  const countdown = AsyncCountdown.fromNanos(endTimestamp);
  // Show the page with the option to cancel
  const page = new Promise<"canceled">((resolve) =>
    pollForTentativeDevicePage({
      cancel: () => {
        countdown.stop();
        resolve("canceled");
      },
      origin: window.origin,
      userNumber,
      remaining: countdown.remainingFormattedAsync(),
    })
  );

  // Poll repeatedly
  const polling = poll(userNumber, connection, () => countdown.hasStopped());

  return Promise.race([page, polling]);
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

// Displays the QR code with given link, in the given container
const displayQR = async ({
  link: text,
  container,
}: {
  link: string;
  container: HTMLElement;
}) => {
  // Dynamically load the QR code module
  const qrCreator: typeof QrCreator | null = (await import("qr-creator"))
    .default;
  if (qrCreator === null) {
    toast.error("Could not load QR code");
    console.error("Could not load qr-creator module");
    return;
  }

  // Retrieve a fitting color from design tokens (defaults to black if for some reason none is found)
  const fill: string = getComputedStyle(
    document.documentElement
  ).getPropertyValue("--vc-brand-purple");

  // Create the QR code
  qrCreator.render(
    {
      text,
      fill,
      radius: 0, // Don't round the squares
      ecLevel: "M", // Medium error correction
      size: 256, // in pixels
    },
    container
  );
};
