import { html, TemplateResult } from "lit-html";
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

  const pageContent = html`
    <p>${copy.dapps_explorer}</p>
    <h1>${copy.try_these_dapps}</h1>
    <button @click=${() => back()} class="c-button">
      Let's get out of here
    </button>
    <hr />
    <ul>
      ${dapps.map((dapp) => dappTemplate(dapp))}
    </ul>
  `;

  return pageContent;
};

export const dappsExplorerPage = renderPage(dappsExplorerTemplate);

// Infer the type of an array's elements
type ElementOf<Arr> = Arr extends readonly (infer ElementOf)[]
  ? ElementOf
  : "argument is not an array";

/* Template for a single dapp */
const dappTemplate = (dapp: ElementOf<typeof dapps>): TemplateResult => {
  return html`
    <li style="display: flex; justify-content: space-between; padding: 1em;">
      <img
        height="64"
        width="64"
        src=${dapp.logo.replace("/img/showcase/", "/icons/")}
        alt=${dapp.name}
      />
      <h1>${dapp.name}</h1>
      ${dapp.oneLiner !== undefined ? html`<p>${dapp.oneLiner}</p>` : undefined}
      <a
        href=${dapp.link}
        target="_blank"
        rel="noopener noreferrer"
        class="c-button c-button--minimal"
        >Open</a
      >
    </li>
  `;
};

/* Show a list of dapps known to use Internet Identity, in a closable modal */
export const dappsExplorer = (): Promise<void> => {
  const i18n = new I18n();
  return new Promise((resolve) =>
    dappsExplorerPage({ i18n, back: () => resolve() })
  );
};
