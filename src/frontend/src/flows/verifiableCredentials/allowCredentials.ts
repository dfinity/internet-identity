import { mkAnchorInput } from "$src/components/anchorInput";
import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import { TemplateResult, html } from "lit-html";

import copyJson from "./allowCredentials.json";

/* A screen prompting the user to allow (or cancel) issuing verified
 * credentials */

const allowCredentialsTemplate = ({
  i18n,
  relyingOrigin,
  providerOrigin,
  consentMessage,
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

  const slot = html`
    <hgroup
      data-page="vc-allow"
      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
    >
      <h1 class="t-title t-title--main">${copy.title}</h1>
    </hgroup>

    ${anchorInput.template}

    <p class="t-paragraph">
      ${copy.allow_start}
      <strong class="t-strong">${providerOrigin}</strong> ${copy.allow_sep_with}
      <strong class="t-strong">${relyingOrigin}</strong>?
    </p>

    <div class="l-stack c-input c-input--readonly">
      <pre class="c-consent-message">${consentMessage}</pre>
    </div>

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
