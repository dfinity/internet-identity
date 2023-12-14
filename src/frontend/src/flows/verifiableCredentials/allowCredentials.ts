import { mkAnchorInput } from "$src/components/anchorInput";
import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { markdownToHTML } from "$src/utils/html";
import { mount, renderPage, TemplateElement } from "$src/utils/lit-html";
import { Chan } from "$src/utils/utils";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { unsafeHTML } from "lit-html/directives/unsafe-html.js";

import { getDapps, KnownDapp } from "../dappsExplorer/dapps";
import { dappTemplate } from "../dappsExplorer/index";

import DOMPurify from "dompurify";

import copyJson from "./allowCredentials.json";

/* A screen prompting the user to allow (or cancel) issuing verified
 * credentials */
const getOrigin = (
  origin: string,
  dapplist: KnownDapp[],
  oneLiner: string
): KnownDapp => {
  let foundDapp = dapplist.find((dapp) => dapp.hasOrigin(origin));
  if (!foundDapp) {
    foundDapp = new KnownDapp({
      name: "Unknown Dapp",
      website: origin,
      logo: "unwnowndapp.png",
      oneLiner: oneLiner,
    });
  }
  return foundDapp;
};

const allowCredentialsTemplate = ({
  i18n,
  relyingOrigin,
  providerOrigin,
  consentMessage: consentMessage_,
  userNumber,
  onAllow,
  onCancel,
  scrollToTop = false,
}: {
  i18n: I18n;
  relyingOrigin: string;
  providerOrigin: string;
  consentMessage: string;
  userNumber?: bigint;
  onAllow: (userNumber: bigint) => void;
  onCancel: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const anchorInput = mkAnchorInput({
    userNumber,
    onSubmit: (userNumber) => onAllow(userNumber),
  });

  const knownDapps = getDapps();
  const consentMessage = new Chan<TemplateElement>(html`${consentMessage_}`);

  // Kickstart markdown parsing & sanitizing; once done, replace the consent message
  void (async () => {
    const parsed = await markdownToHTML(consentMessage_);
    const sanitized = await DOMPurify.sanitize(parsed);
    consentMessage.send(unsafeHTML(sanitized));
  })();

  const originDapp = getOrigin(providerOrigin, knownDapps, `${copy.issued_by}`);

  const relyingDapp = getOrigin(
    relyingOrigin,
    knownDapps,
    `${copy.relying_party}`
  );

  const slot = html`
    <hgroup
      data-page="vc-allow"
      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
    >
      <h1 class="t-title t-title--main">${copy.title}</h1>
    </hgroup>
    <article class="l-stack c-card c-card--consent">
      <div class="t-formatted t-formatted--monospace">
        ${asyncReplace(consentMessage)}
      </div>
    </article>
    ${anchorInput.template}
    <h2 class="c-card__label l-stack">${copy.allow_start}</h2>
    <ul class="c-action-list">
      <li class="c-action-list__item">${dappTemplate(originDapp)}</li>
      <li class="c-action-list__item">${dappTemplate(relyingDapp)}</li>
    </ul>

    <div class="c-button-group">
      <button
        data-action="cancel"
        class="c-button c-button--secondary"
        @click="${() => onCancel()}"
      >
        ${copy.cancel}
      </button>
      <button
        data-action="allow"
        class="c-button"
        @click="${() => anchorInput.submit()}"
      >
        ${copy.allow}
      </button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const allowCredentialsPage = renderPage(allowCredentialsTemplate);

// Prompt to allow verifying credentials
export const allowCredentials = ({
  relyingOrigin,
  providerOrigin,
  consentMessage,
  userNumber,
}: {
  relyingOrigin: string;
  providerOrigin: string;
  consentMessage: string;
  userNumber?: bigint;
}): Promise<{ tag: "allowed"; userNumber: bigint } | { tag: "canceled" }> => {
  return new Promise((resolve) =>
    allowCredentialsPage({
      i18n: new I18n(),
      relyingOrigin,
      providerOrigin,
      consentMessage,
      userNumber,
      onAllow: (userNumber) => resolve({ tag: "allowed", userNumber }),
      onCancel: () => resolve({ tag: "canceled" }),
      scrollToTop: true,
    })
  );
};
