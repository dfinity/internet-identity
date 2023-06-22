import { mainWindow } from "$src/components/mainWindow";
import { mount, renderPage } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";

export const promptRecoveryTemplate = ({
  onUsePhrase,
  onUseDevice,
  onForgotAnchor,
  cancel,
  scrollToTop = false,
}: {
  onUsePhrase: () => void;
  onUseDevice: () => void;
  onForgotAnchor: () => void;
  cancel: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const slot = html`
    <hgroup
      data-page="prompt-recovery-type"
      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
    >
      <h1 class="t-title t-title--main">Lost access?</h1>
      <p class="t-paragraph">Choose the option that best fits your needs.</p>
    </hgroup>
    <div class="l-stack">
      <button
        @click=${() => onUsePhrase()}
        data-action="recover-with-phrase"
        class="c-button c-button--secondary"
      >
        Use recovery phrase
      </button>
      <button
        @click=${() => onUseDevice()}
        data-action="recover-with-device"
        class="c-button c-button--secondary"
      >
        Use recovery device
      </button>
      <button
        @click=${() => onForgotAnchor()}
        data-action="lost-identity"
        class="c-button c-button--secondary"
      >
        Forgot Internet Identity
      </button>
      <button
        @click=${() => cancel()}
        data-action="cancel"
        class="c-button c-button--primary l-stack"
      >
        Back to home
      </button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const promptRecoveryPage = renderPage(promptRecoveryTemplate);

export const promptRecovery = (): Promise<
  "phrase" | "device" | "forgotten" | "cancel"
> => {
  return new Promise((resolve) => {
    promptRecoveryPage({
      onUsePhrase: () => resolve("phrase"),
      onUseDevice: () => resolve("device"),
      onForgotAnchor: () => resolve("forgotten"),
      cancel: () => resolve("cancel"),
      scrollToTop: true,
    });
  });
};
