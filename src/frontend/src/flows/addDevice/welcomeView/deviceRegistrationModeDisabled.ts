import { html, render } from "lit-html";
import { mainWindow } from "../../../components/mainWindow";
import { LEGACY_II_URL_NO_PROTOCOL } from "../../../config";

const deviceRegistrationDisabledInfoTemplate = ({
  userNumber,
  cancel,
  retry,
}: {
  userNumber: bigint;
  cancel: () => void;
  retry: () => void;
}) => {
  const pageContentSlot = html` <article>
    <hgroup>
      <h1 class="t-title t-title--main">
        Enroll this device from a registered device
      </h1>
      <p class="t-lead">
        If you want to add this device, follow the instructions below
        <strong class="t-strong"
          >on a device that Internet Identity already trusts:</strong
        >
      </p>
    </hgroup>
    <ol class="c-list c-list--numbered l-stack">
      <li>
        Log into
        <strong class="t-strong">${LEGACY_II_URL_NO_PROTOCOL}</strong> with your
        Identity Anchor (<strong class="t-strong">${userNumber}</strong>)
      </li>
      <li>
        Once you are logged in, click “<strong class="t-string"
          >Add new device</strong
        >”
      </li>
      <li>Select “<strong class="t-string">Browser</strong>”</li>
    </ol>
    <p class="t-paragraph t-strong">Then, press Retry below.</p>
    <div class="l-stack">
      <button
        id="deviceRegModeDisabledRetry"
        class="c-button"
        @click=${() => retry()}
      >
        Retry
      </button>
      <button
        id="deviceRegModeDisabledCancel"
        class="c-button c-button--secondary"
        @click=${() => cancel()}
      >
        Cancel
      </button>
    </div>
  </article>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const deviceRegistrationDisabledInfoPage = (
  props: Parameters<typeof deviceRegistrationDisabledInfoTemplate>[0],
  container?: HTMLElement
): void => {
  const contain =
    container ?? (document.getElementById("pageContent") as HTMLElement);
  render(deviceRegistrationDisabledInfoTemplate(props), contain);
};

/**
 * Error page which is shown if the identy anchor does not have device registration mode enabled.
 * It shows instructions to the user on how to continue.
 */
export const deviceRegistrationDisabledInfo = (
  userNumber: bigint
): Promise<"canceled" | "retry"> => {
  return new Promise((resolve) => {
    return deviceRegistrationDisabledInfoPage({
      userNumber,
      cancel: () => resolve("canceled"),
      retry: () => resolve("retry"),
    });
  });
};
