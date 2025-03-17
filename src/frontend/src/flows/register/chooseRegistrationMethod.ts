import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { renderPage } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";
import copyJson from "./chooseRegistrationMethod.json";

export const chooseRegistrationMethodTemplate = ({
  chosenRegistrationMethod,
  i18n,
  cancel,
}: {
  chosenRegistrationMethod: (method: "google" | "passkey") => void;
  i18n: I18n;
  cancel: () => void;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const slot = html`
    <hgroup>
      <h1 class="t-title t-title--main">Choose your Method</h1>
      <p class="t-paragraph">To create your Identity</p>
    </hgroup>
    <button
      @click=${() => chosenRegistrationMethod("passkey")}
      data-action="choose-passkey"
      class="c-button l-stack"
    >
      ${copy.continue_with_passkey}
    </button>
    <button
      @click=${() => chosenRegistrationMethod("google")}
      data-action="choose-google"
      class="c-button l-stack"
    >
      ${copy.continue_with_google}
    </button>
    <button
      @click=${() => cancel()}
      data-action="cancel"
      class="c-button c-button--textOnly"
    >
      ${copy.cancel}
    </button>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const chooseRegistrationMethodPage = renderPage(
  chooseRegistrationMethodTemplate
);

export const chooseRegistrationMethod = async (): Promise<
  "google" | "passkey" | "canceled"
> => {
  return new Promise((resolve) => {
    return chooseRegistrationMethodPage({
      chosenRegistrationMethod: (method: "google" | "passkey") =>
        resolve(method),
      i18n: new I18n(),
      cancel: () => resolve("canceled"),
    });
  });
};
