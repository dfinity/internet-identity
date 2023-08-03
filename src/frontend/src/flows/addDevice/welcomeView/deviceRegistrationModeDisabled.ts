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
      <h1 class="t-title t-title--main">Add this device as a passkey</h1>
      <p class="t-lead">
        If this is your first time connecting to Internet Identity on this
        device, follow these steps to continue:
      </p>
    </hgroup>
    <ol class="c-list c-list--numbered l-stack">
      <li>
        Connect to ${LEGACY_II_URL_NO_PROTOCOL} on a recognized device using
        Internet Identity ${userNumber}
      </li>
      <li>
        Once you are connected, select “<strong class="t-string"
          >Add a new passkey</strong
        >”
      </li>
    </ol>
    <p class="t-paragraph">
      Then, press <strong class="t-strong">Refresh</strong> below.
    </p>
    <div class="l-stack">
      <button
        id="deviceRegModeDisabledRetry"
        class="c-button"
        @click=${() => retry()}
      >
        Refresh
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
