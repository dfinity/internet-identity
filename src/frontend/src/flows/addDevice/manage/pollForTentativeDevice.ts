import { isNullish, nonNullish } from "@dfinity/utils";
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
import { I18n } from "../../../i18n";
import { addDeviceLink } from "../../../utils/addDeviceLink";
import { AsyncCountdown } from "../../../utils/countdown";
import { AuthenticatedConnection } from "../../../utils/iiConnection";
import { mount, renderPage, withRef } from "../../../utils/lit-html";
import { delayMillis } from "../../../utils/utils";

import copyJson from "./pollForTentativeDevice.json";

const pollForTentativeDeviceTemplate = ({
  userNumber,
  cancel,
  remaining,
  origin,
  i18n,
}: {
  userNumber: bigint;
  cancel: () => void;
  remaining: AsyncIterable<string>;
  origin: string;
  i18n: I18n;
}) => {
  const copy = i18n.i18n(copyJson);
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
      toast.error(copy.could_not_copy_link);
      console.error(copy.could_not_copy_link, e);
    }
  };

  const pageContentSlot = html`
    <hgroup>
      <h2 class="t-paragraph">${copy.identity_anchor} ${userNumber}</h2>
      <h1 class="t-title t-title--main l-stack--none">
        ${copy.add_trusted_device}
      </h1>
      <p class="t-lead">${copy.follow_steps_on_new_device}</p>
    </hgroup>
    <div
      class="t-centered c-qrcode l-stack"
      ${mount((container) =>
        container instanceof HTMLElement
          ? displayQR({ link, container })
          : undefined
      )}
    ></div>
    <div class="t-wrap c-input c-input--with-button c-input--centered t-weak">
      <span data-role="add-device-link">${link}</span>
      <button
        ${ref(linkCopyElement)}
        @click=${() => copyLink()}
        aria-label=${copy.copy_to_clipboard}
        title=${copy.copy_to_clipboard}
        id="seedCopy"
        data-action="copy-link"
        class="c-button__icon"
      >
        <span>Copy</span>
        ${copyIcon} ${checkmarkIcon}
      </button>
    </div>

    <ol class="c-list c-list--numbered l-stack">
      <li>${copy.scan_qr_or_visit_link}</li>
      <li>${copy.sign_in_using_device}</li>
    </ol>

    <button
      @click=${() => cancel()}
      id="cancelAddRemoteDevice"
      class="c-button c-button--secondary l-stack"
    >
      ${copy.cancel}
    </button>
    <p class="t-paragraph">
      ${copy.time_remaining}:
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
export const pollForTentativeDevice = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  endTimestamp: Timestamp
): Promise<DeviceData | "timeout" | "canceled"> => {
  const i18n = new I18n();
  const countdown: AsyncCountdown<DeviceData | "timeout" | "canceled"> =
    AsyncCountdown.fromNanos(endTimestamp);
  // Display the page with the option to cancel
  pollForTentativeDevicePage({
    cancel: () => countdown.stop("canceled"),
    origin: window.origin,
    userNumber,
    remaining: countdown.remainingFormattedAsync(),
    i18n,
  });

  // Poll repeatedly
  void (async () => {
    const result = await poll(userNumber, connection, () =>
      countdown.hasStopped()
    );
    countdown.stop(result);
  })();

  // Map the AsyncCountdown timeout symbol to something the caller can use
  const result = await countdown.wait();
  return result === AsyncCountdown.timeout ? "timeout" : result;
};

const poll = (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  shouldStop: () => boolean
): Promise<DeviceData> =>
  // eslint-disable-next-line no-async-promise-executor
  new Promise(async (resolve) => {
    while (!shouldStop()) {
      const anchorInfo = await connection.getAnchorInfo();
      const tentativeDevice =
        anchorInfo.device_registration[0]?.tentative_device[0];
      if (nonNullish(tentativeDevice)) {
        resolve(tentativeDevice);
        return;
      }

      // Debounce a little; in practice won't be noticed by users but
      // will avoid hot looping in case the op becomes near instantaneous.
      await delayMillis(100);
    }
  });

// Dynamically load the QR code module
const loadQrCreator = async (): Promise<typeof QrCreator | undefined> => {
  try {
    return (await import("qr-creator")).default;
  } catch (e) {
    console.error(e);
    return undefined;
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
  // Load the QR code library
  const qrCreator = await loadQrCreator();
  if (isNullish(qrCreator)) {
    toast.error("Could not load QR code");
    console.error("Could not load qr-creator module");
    return;
  }

  // Retrieve a fitting color CSS (defaults to black if for some reason none is set)
  const fill: string = getComputedStyle(
    document.documentElement
  ).getPropertyValue("currentColor");

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
