import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import { TemplateResult, html } from "lit-html";
import copyJson from "./chooseRegistrationMethod.json";

export const chooseRegistrationMethodTemplate = ({
  chosenRegistrationMethod,
  i18n,
  cancel,
  scrollToTop,
}: {
  chosenRegistrationMethod: (method: "google" | "passkey") => void;
  i18n: I18n;
  cancel: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const slot = html`
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">${copy.create_internet_identity}</h1>
      <p class="t-paragraph">${copy.to_continue}</p>
    </hgroup>
    <button
      @click=${() => chosenRegistrationMethod("passkey")}
      data-action="construct-passkey-identity"
      class="c-button l-stack"
    >
      ${copy.continue_with_passkey}
    </button>
    <button
      @click=${() => chosenRegistrationMethod("google")}
      data-action="construct-google-identity"
      class="c-button c-button--secondary"
    >
      ${copy.continue_with_google}
    </button>
    <button
      @click=${() => cancel()}
      data-action="cancel"
      class="c-button l-stack c-button--textOnly"
    >
      ${copy.cancel}
    </button>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: true,
    slot,
  });
};

export const chooseRegistrationMethodPage = renderPage(
  chooseRegistrationMethodTemplate
);

export const chooseRegistrationMethod = (): Promise<
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
