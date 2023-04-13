import { html, TemplateResult } from "lit-html";
import { closeIcon } from "../../components/icons";
import { mainWindow } from "../../components/mainWindow";
import { I18n } from "../../i18n";
import { renderPage } from "../../utils/lit-html";

// The list of dapps. This is derived from https://github.com/dfinity/portal:
// * Only dapps using II are used
// * All relevant logos are copied to II's assets
// * Some logos are converted to webp
import dapps from "./dapps.json";

import copyJson from "./copy.json";

/* Template for the explorer containing all dapps */
const dappsExplorerTemplate = ({
  i18n,
  back,
}: {
  i18n: I18n;
  back: () => void;
}) => {
  const copy = i18n.i18n(copyJson);
  const staticCopy = i18n.staticLang(copyJson);

  const pageContent = html`
    <button
      class="c-card__close"
      aria-label=${staticCopy.back_to_the_previous_page}
      @click=${() => back()}
    >
      ${closeIcon}
    </button>
    <hgroup>
      <h2 class="t-title t-title--discrete">${copy.dapps_explorer}</h2>
      <h1 class="t-title">${copy.try_these_dapps}</h1>
    </hgroup>
    <div class="c-action-list">${dapps.map((dapp) => dappTemplate(dapp))}</div>
  `;

  const wrappedPageContent = mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContent,
  });

  return wrappedPageContent;
};

export const dappsExplorerPage = renderPage(dappsExplorerTemplate);

// Infer the type of an array's elements
type ElementOf<Arr> = Arr extends readonly (infer ElementOf)[]
  ? ElementOf
  : "argument is not an array";

/* Template for a single dapp */
const dappTemplate = ({
  link,
  logo,
  name,
  oneLiner,
}: ElementOf<typeof dapps>): TemplateResult => {
  return html`
    <a
      href=${link}
      target="_blank"
      class="c-action-list__item"
      rel="noopener noreferrer"
    >
      <div class="c-action-list__icon" aria-hidden="true">
        <img
          src=${logo.replace("/img/showcase/", "/icons/")}
          alt=${name}
          loading="lazy"
        />
      </div>
      <div class="c-action-list__label c-action-list__label--stacked">
        <h3 class="t-title t-title--list">${name}</h3>
        ${oneLiner !== undefined
          ? html`<p class="t-weak">${oneLiner}</p>`
          : undefined}
      </div>
      <span class="c-action-list__action"> ↗ </span>
    </a>
  `;
};

/* Show a list of dapps known to use Internet Identity, in a closable modal */
export const dappsExplorer = (): Promise<void> => {
  const i18n = new I18n();
  return new Promise((resolve) =>
    dappsExplorerPage({ i18n, back: () => resolve() })
  );
};
