import { mainWindow } from "$src/components/mainWindow";
import { LEGACY_II_URL_NO_PROTOCOL } from "$src/config";
import { renderPage } from "$src/utils/lit-html";
import { html } from "lit-html";

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
        Use another Passkey to register this device
      </h1>
      <p class="t-lead">
        If you want to use this device to create a new Passkey, connect to
        Internet Identity with an existing Passkey. Follow these instructions:
      </p>
    </hgroup>
    <ol class="c-list c-list--numbered l-stack">
      <li>
        Log into
        <strong class="t-strong">${LEGACY_II_URL_NO_PROTOCOL}</strong> with your
        Internet Identity (<strong class="t-strong">${userNumber}</strong>)
        using an existing Passkey
      </li>
      <li>
        Once you are logged in, click “<strong class="t-string"
          >Add new Passkey</strong
        >”
      </li>
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

export const deviceRegistrationDisabledInfoPage = renderPage(
  deviceRegistrationDisabledInfoTemplate
);

/**
 * Error page which is shown if the identy anchor does not have device registration mode enabled.
 * It shows instructions to the user on how to continue.
 */
export const deviceRegistrationDisabledInfo = (
  userNumber: bigint
): Promise<"canceled" | "retry"> => {
  return new Promise((resolve) =>
    deviceRegistrationDisabledInfoPage({
      userNumber,
      cancel: () => resolve("canceled"),
      retry: () => resolve("retry"),
    })
  );
};
