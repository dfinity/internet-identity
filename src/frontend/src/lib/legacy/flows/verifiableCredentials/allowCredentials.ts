import { mkAnchorInput } from "$lib/templates/anchorInput";
import { mainWindow } from "$lib/templates/mainWindow";
import { I18n } from "$lib/legacy/i18n";
import { markdownToHTML } from "$lib/utils/html";
import { mount, renderPage, TemplateElement } from "$lib/utils/lit-html";
import { Chan } from "$lib/utils/utils";
import { html, TemplateResult } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { unsafeHTML } from "lit-html/directives/unsafe-html.js";

import { getDapps, KnownDapp } from "../dappsExplorer/dapps";

import DOMPurify from "dompurify";

import copyJson from "./allowCredentials.json";

import unknownDappLogo from "$lib/legacy/assets/unknowndapp.png?url";

/*
 * Get the dapp that corresponds to the origin, or create a new one if it's
 * unknown
 */
const getOrigin = (origin: string, dapplist: KnownDapp[]): KnownDapp => {
  let foundDapp = dapplist.find((dapp) => dapp.hasOrigin(origin));

  if (!foundDapp) {
    foundDapp = new KnownDapp({
      name: origin,
      website: origin,
      logo: unknownDappLogo,
    });
  }

  return foundDapp;
};

/* A screen prompting the user to allow (or cancel) issuing verified
 * credentials */
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

  const presentCredential = () => {
    if (userNumber === undefined) {
      anchorInput.submit();
      return;
    }
    onAllow(userNumber);
  };

  const knownDapps = getDapps();
  const consentMessage = new Chan<TemplateElement>(html`${consentMessage_}`);

  // Kickstart markdown parsing & sanitizing; once done, replace the consent message
  void (async () => {
    const parsed = await markdownToHTML(consentMessage_);
    const sanitized = await DOMPurify.sanitize(parsed);
    consentMessage.send(unsafeHTML(sanitized));
  })();

  const originDapp = getOrigin(providerOrigin, knownDapps);
  const relyingDapp = getOrigin(relyingOrigin, knownDapps);

  // copy.relying_party as string
  const slot = html`
    <hgroup
      data-page="vc-allow"
      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
    >
      <h1 class="t-title t-title--main">${copy.title}</h1>
    </hgroup>

    <div class="c-card c-card--narrow c-card--warning l-stack">
      <article class="c-card--consent">
        <div class="t-formatted t-formatted--monospace">
          ${asyncReplace(consentMessage)}
        </div>
      </article>
      <div class="l-horizontal l-stack--small">
        <div class="c-card__logo" aria-hidden="true">
          <img
            src=${originDapp.logoSrc}
            alt=${originDapp.name}
            loading="lazy"
          />
        </div>
        <div class="l-vertical">
          <div class="c-card__label">
            <h3>${copy.issued_by}</h3>
          </div>
          <h2 class="t-title" data-role="issuer">${originDapp.name}</h2>
        </div>
      </div>
    </div>

    ${userNumber === undefined ? anchorInput.template : ""}

    <div class="c-separator l-stack">
      <div class="c-separator__dot--tiny"></div>
      <div class="c-separator__dot--small"></div>
      <div class="c-separator__dot"></div>
      <div class="c-separator__dot--small"></div>
      <div class="c-separator__dot--tiny"></div>
    </div>

    <div class="c-card c-card--narrow l-stack">
      <div class="l-horizontal">
        <div class="c-card__logo" aria-hidden="true">
          <img
            src=${relyingDapp.logoSrc}
            alt=${relyingDapp.name}
            loading="lazy"
          />
        </div>
        <div class="l-vertical">
          <div class="c-card__label">
            <h3>${copy.relying_party}</h3>
          </div>
          <h2 class="t-title" data-role="relying-party">${relyingDapp.name}</h2>
        </div>
      </div>
    </div>

    <p class="t-paragraph--weak t-centered l-stack">${copy.note}</p>

    <div class="c-button-group--stack">
      <button
        data-action="allow"
        class="c-button"
        @click="${presentCredential}"
      >
        ${copy.present_credential}
      </button>
      <button
        data-action="cancel"
        class="c-button c-button--secondary"
        @click="${() => onCancel()}"
      >
        ${copy.cancel}
      </button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: true,
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
    }),
  );
};
