import { DeviceData } from "$generated/internet_identity_types";
import { securityKeyIcon, seedPhraseIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { renderPage } from "$src/utils/lit-html";
import { hasRecoveryKey, hasRecoveryPhrase } from "$src/utils/recoveryDevice";
import { html, TemplateResult } from "lit-html";

export type RecoveryMechanism = "securityKey" | "seedPhrase";

const chooseRecoveryMechanismTemplate = ({
  title,
  message,
  cancelText,

  disablePhrase,
  disableKey,
  pick,
  cancel,
}: {
  title: TemplateResult;
  message: TemplateResult;
  cancelText: TemplateResult;

  disablePhrase: boolean;
  disableKey: boolean;
  pick: (recovery: "securityKey" | "seedPhrase") => void;
  cancel: () => void;
}) => {
  const pageContentSlot = html`
    <article>
      <hgroup>
        <h1 class="t-title t-title--main">${title}</h1>
        <p class="t-lead">${message}</p>
      </hgroup>
      <div class="l-horizontal l-stack">
        <button
          ?disabled=${disablePhrase}
          class="c-button c-button--secondary"
          @click=${() => pick("seedPhrase")}
          id="seedPhrase"
        >
          <span aria-hidden="true">${seedPhraseIcon}</span>
          <div class="t-strong">Recovery Phrase</div>
          <div class="t-weak">Use secret phrase</div>
        </button>
        <button
          ?disabled=${disableKey}
          class="c-button c-button--secondary"
          @click=${() => pick("securityKey")}
          id="securityKey"
        >
          <span aria-hidden="true">${securityKeyIcon}</span>
          <div class="t-strong">Recovery Device</div>
          <div class="t-weak">Use FIDO device</div>
        </button>
      </div>
      <div class="l-stack">
        <button @click=${() => cancel()} id="skipRecovery" class="c-button">
          ${cancelText}
        </button>
      </div>
    </article>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: pageContentSlot,
  });
};

type TemplateProps = Parameters<typeof chooseRecoveryMechanismTemplate>[0];
export type ChooseRecoveryProps = Pick<
  TemplateProps,
  "title" | "message" | "cancelText"
>;

export const chooseRecoveryMechanismPage = renderPage(
  chooseRecoveryMechanismTemplate
);

export const chooseRecoveryMechanism = ({
  devices,
  title,
  message,
  cancelText,
}: {
  devices: Omit<DeviceData, "alias">[];
} & ChooseRecoveryProps): Promise<RecoveryMechanism | "canceled"> => {
  return new Promise((resolve) => {
    return chooseRecoveryMechanismPage({
      disablePhrase: hasRecoveryPhrase(devices),
      disableKey: hasRecoveryKey(devices),
      pick: resolve,
      cancel: () => resolve("canceled"),

      title,
      message,
      cancelText,
    });
  });
};
