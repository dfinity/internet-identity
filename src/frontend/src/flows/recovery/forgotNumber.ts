import { mainWindow } from "$src/components/mainWindow";
import { mount, renderPage } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";

export const forgotNumberTemplate = ({
  cancel,
  scrollToTop = false,
}: {
  cancel: () => void;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const slot = html`
    <hgroup
      data-page="forgot-identity-number"
      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
    >
      <h1 class="t-title t-title--main">Forgot your Internet Identity?</h1>
    </hgroup>
    <section class="c-marketing-block">
      <p class="t-paragraph">
        Your Internet Identity is the number that was assigned to you when you
        signed up. You probably took a screenshot or wrote it down. If you
        forgot it, you can:
      </p>
      <ul class="c-list c-list--bulleted">
        <li>
          Review your screenshots from the time that you created an Internet
          Identity
        </li>
        <li>
          Check your recovery phrase, your Internet Identity is the first number
          in your recovery phrase
        </li>
      </ul>
    </section>

    <div class="l-stack">
      <button
        @click=${() => cancel()}
        data-action="cancel"
        class="c-button c-button--primary"
      >
        Back to Home
      </button>
    </div>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const forgotNumberPage = renderPage(forgotNumberTemplate);

export const forgotNumber = (): Promise<"cancel"> => {
  return new Promise((resolve) => {
    forgotNumberPage({
      cancel: () => resolve("cancel"),
      scrollToTop: true,
    });
  });
};
