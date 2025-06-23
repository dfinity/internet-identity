import { closeIcon, externalLinkIcon } from "$lib/templates/icons";
import { mainWindow } from "$lib/templates/mainWindow";
import { I18n } from "$lib/legacy/i18n";
import { mount, renderPage } from "$lib/utils/lit-html";
import { TemplateResult, html } from "lit-html";

import { KnownDapp } from "./dapps";

import { nonNullish } from "@dfinity/utils";
import copyJson from "./copy.json";

/* Template for the explorer containing all dapps */
const dappsExplorerTemplate = ({
  dapps,
  i18n,
  back,
  scrollToTop = false,
}: {
  dapps: KnownDapp[];
  i18n: I18n;
  back: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}) => {
  const copy = i18n.i18n(copyJson);

  const pageContent = html`
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <div class="c-card__label"><h2>${copy.dapps_explorer}</h2></div>
      <h1 class="t-title">${copy.try_these_dapps}</h1>
    </hgroup>
    <button
      class="c-card__close"
      aria-label=${copy.back_to_the_previous_page}
      @click=${() => back()}
    >
      ${closeIcon}
    </button>
    <div class="c-action-list">
      ${dapps.map((dapp) => dappTemplateLink(dapp))}
    </div>
    <p class="t-paragraph t-centered">
      ${copy.add_your_dapp}
      <a
        href="https://identitysupport.dfinity.org/hc/en-us/articles/16195954040724"
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

export type DappTemplateArgs = {
  logoSrc: string;
  name: string;
  oneLiner?: string;
  oneLinerAboveTitle?: boolean;
};

export const dappTemplate = ({
  logoSrc,
  name,
  oneLiner,
  oneLinerAboveTitle = false,
}: DappTemplateArgs): TemplateResult => {
  return html`
    <div class="c-action-list__icon" aria-hidden="true">
      <img src=${logoSrc} alt=${name} loading="lazy" />
    </div>
    <div
      class="c-action-list__label c-action-list__label--stacked${oneLinerAboveTitle ===
      true
        ? " c-action-list__label--inverted"
        : ""}"
    >
      <h3 class="t-title t-title--list">${name}</h3>
      ${nonNullish(oneLiner)
        ? html`<p class="t-weak">${oneLiner}</p>`
        : undefined}
    </div>
  `;
};

/* Template for a single dapp */
const dappTemplateLink = ({
  website,
  logoSrc,
  name,
  oneLiner,
}: KnownDapp): TemplateResult => {
  return html`
    <a
      href=${website}
      target="_blank"
      class="c-action-list__item"
      rel="noopener noreferrer"
    >
      ${dappTemplate({ logoSrc, name, oneLiner } as KnownDapp)}
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
  dapps: KnownDapp[];
}): Promise<void> => {
  const i18n = new I18n();
  return new Promise((resolve) =>
    dappsExplorerPage({
      dapps,
      i18n,
      back: () => resolve(),
      scrollToTop: true,
    }),
  );
};
