import { mainWindow } from "$src/components/mainWindow";
import { mount, renderPage } from "$src/utils/lit-html";
import { TemplateResult, html } from "lit-html";

/* VC credential allow/deny screen */

const allowTemplate = ({
  relyingOrigin,
  providerOrigin,
  onAllow,
  onCancel,
  scrollToTop = false,
}: {
  relyingOrigin: string;
  providerOrigin: string;
  onAllow: () => void;
  onCancel: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const slot = html`
    <hgroup ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}>
      <h1 class="t-title t-title--main">Credential Access Request</h1>
    </hgroup>
    <p class="t-paragraph">
      Allow verifying credential
      <strong class="t-strong">${providerOrigin}</strong> with
      <strong class="t-strong">${relyingOrigin}</strong>?
    </p>

    <div class="c-button-group">
      <button
        data-action="cancel"
        class="c-button c-button--secondary"
        @click="${() => onCancel()}"
      >
        Cancel
      </button>
      <button data-action="allow" class="c-button" @click="${() => onAllow()}">
        Allow
      </button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const allowPage = renderPage(allowTemplate);

// Prompt to allow verifying credentials
export const allow = ({
  relyingOrigin,
  providerOrigin,
}: {
  relyingOrigin: string;
  providerOrigin: string;
}): Promise<"allowed" | "canceled"> => {
  return new Promise((resolve) =>
    allowPage({
      relyingOrigin,
      providerOrigin,
      onAllow: () => resolve("allowed"),
      onCancel: () => resolve("canceled"),
      scrollToTop: true,
    })
  );
};
