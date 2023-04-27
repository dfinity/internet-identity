import { closeIcon, externalLinkIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { renderPage } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";

import { DappDescription } from "./dapps";

import copyJson from "./copy.json";

/* Template for the explorer containing all dapps */
const dappsExplorerTemplate = ({
  dapps,
  i18n,
  back,
}: {
  dapps: DappDescription[];
  i18n: I18n;
  back: () => void;
}) => {
  const copy = i18n.i18n(copyJson);

  const pageContent = html`
    <hgroup>
      <h2 class="t-title t-title--discrete">${copy.dapps_explorer}</h2>
      <h1 class="t-title">${copy.try_these_dapps}</h1>
    </hgroup>
    <button
      class="c-card__close"
      aria-label=${copy.back_to_the_previous_page}
      @click=${() => back()}
    >
      ${closeIcon}
    </button>
    <div class="c-action-list">${dapps.map((dapp) => dappTemplate(dapp))}</div>
    <p class="t-paragraph t-centered">
      ${copy.add_your_dapp}
      <a
        href="https://github.com/dfinity/internet-identity/issues/new"
        target="_blank"
        rel="noopener noreferrer"
      >
        ${copy.get_in_touch}</a
      >
    </p>
  `;

  const wrappedPageContent = mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContent,
  });

  return wrappedPageContent;
};

export const dappsExplorerPage = renderPage(dappsExplorerTemplate);

/* Template for a single dapp */
const dappTemplate = ({
  website,
  logo,
  name,
  oneLiner,
}: DappDescription): TemplateResult => {
  return html`
    <a
      href=${website}
      target="_blank"
      class="c-action-list__item"
      rel="noopener noreferrer"
    >
      <div class="c-action-list__icon" aria-hidden="true">
        <img src=${logo} alt=${name} loading="lazy" />
      </div>
      <div class="c-action-list__label c-action-list__label--stacked">
        <h3 class="t-title t-title--list">${name}</h3>
        ${oneLiner !== undefined
          ? html`<p class="t-weak">${oneLiner}</p>`
          : undefined}
      </div>
      <span class="c-action-list__action"
        ><i class="c-icon c-icon--circle">${externalLinkIcon}</i></span
      >
    </a>
  `;
};

/* Show a list of dapps known to use Internet Identity, in a closable modal */
export const dappsExplorer = ({
  dapps,
}: {
  dapps: DappDescription[];
}): Promise<void> => {
  const i18n = new I18n();
  return new Promise((resolve) =>
    dappsExplorerPage({ dapps, i18n, back: () => resolve() })
  );
};
