import { caretDownIcon, verifyIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { KnownDapp } from "$src/flows/dappsExplorer/dapps";
import { I18n } from "$src/i18n";
import { IIWebAuthnIdentity } from "$src/utils/iiConnection";
import { mount, renderPage } from "$src/utils/lit-html";
import { Chan } from "$src/utils/utils";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { vcStepper } from "./stepper";

/* Anchor construction component (for creating WebAuthn credentials) */

const promptTemplate = ({
  i18n,
  cancel,
  userNumber,
  knownDapp,
  scrollToTop = false,
}: {
  i18n: I18n;
  cancel: () => void;
  userNumber: bigint;
  knownDapp: KnownDapp;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const slot = html`
    ${vcStepper({ current: "prompt" })}
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">${userNumber},</h1>
      <div class="l-stack">
      ${mkChasm({
        image: knownDapp.logoSrc,
        name: knownDapp.name,
        message: html`Some context about ${knownDapp.name}`,
      })}
      </div>
      <p class="t-paragraph">
        This app would like to:
        <ul>
            <li>
                <div class="l-stack" style="display: flex; align-items: center; gap: 1em;">${verifyIcon} Anonymously verify that you hold an 8 year neuron</div>
            </li>
        </ul>
      </p>
    </hgroup>
    <div class="c-button-group">
      <button
        type="button"
        @click=${() => cancel()}
        class="c-button c-button--secondary"
      >
        Cancel
      </button>
      <button class="c-button" class="c-button c-button--primary">Allow</button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const promptPage = renderPage(promptTemplate);

// Prompt the user to create a WebAuthn identity
export const prompt = ({
  userNumber,
  knownDapp,
}: {
  userNumber: bigint;
  knownDapp: KnownDapp;
}): Promise<IIWebAuthnIdentity | "canceled"> => {
  return new Promise((resolve) =>
    promptPage({
      i18n: new I18n(),
      userNumber,
      knownDapp,
      cancel: () => resolve("canceled"),
      scrollToTop: true,
    })
  );
};

/** Options to display a "chasm" in the authbox */
type ChasmOpts = {
  image: string;
  name: string;
  message: TemplateResult;
};

const mkChasm = ({ image, name, message }: ChasmOpts): TemplateResult => {
  /* Toggle the chasm open/closed */
  const ariaExpanded = new Chan(false);
  const chasmToggle = () => ariaExpanded.send(!ariaExpanded.latest);
  const btnFlipped = ariaExpanded.map((expanded) =>
    expanded ? "c-chasm__button--flipped" : undefined
  );

  return html`
    <span
      id="alternative-origin-chasm-toggle"
      style="cursor: pointer;"
      class="t-action"
      data-action="toggle-chasm"
      @click=${() => chasmToggle()}
    >
      <div style="display: flex; align-items: center; gap: 0.5em;">
        <img style="height: 2em;" src=${image} alt="" />
        <strong class="t-strong">${name}</strong>
        <span class="t-link__icon c-chasm__button ${asyncReplace(btnFlipped)}"
          >${caretDownIcon}</span
        >
      </div>
    </span>
    <div
      class="c-chasm c-chasm--title"
      aria-expanded=${asyncReplace(ariaExpanded)}
    >
      <div class="c-chasm__inner">
        <div class="c-chasm__arrow c-chasm__arrow-left"></div>
        <div class="t-weak c-chasm__content">
          <p class="t-paragraph t-paragraph--weak">${message}</p>
        </div>
      </div>
    </div>
  `;
};
