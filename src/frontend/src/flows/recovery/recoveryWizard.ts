import { withLoader } from "$src/components/loader";
import { mainWindow } from "$src/components/mainWindow";
import { I18n } from "$src/i18n";
import { mount, renderPage } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";

import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { setupRecovery } from "./setupRecovery";

import { infoIconNaked } from "$src/components/icons";
import copyJson from "./recoveryWizard.json";

/* Phrase creation kick-off screen */

const addPhraseTemplate = ({
  ok,
  cancel,
  cancelText,
  i18n,
  scrollToTop = false,
}: {
  ok: () => void;
  cancel: () => void;
  cancelText: string;
  i18n: I18n;
  /* put the page into view */
  scrollToTop?: boolean;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);
  const slot = html`
    <hgroup
      data-page="add-recovery-phrase"
      ${scrollToTop ? mount(() => window.scrollTo(0, 0)) : undefined}
    >
      <div class="c-card__label c-card__label--hasIcon">
        <i class="c-card__icon c-icon c-icon--info__flipped c-icon--inline"
          >${infoIconNaked}</i
        >
        <h2>${copy.label}</h2>
      </div>
      <h1 class="t-title t-title--main">${copy.title}</h1>
      <p class="t-paragraph">${copy.paragraph}</p>
    </hgroup>
    <div class="l-stack">
      <button @click=${() => ok()} data-action="next" class="c-button">
        ${copy.ok}
      </button>
      <button
        @click=${() => cancel()}
        data-action="cancel"
        class="c-button c-button--secondary"
      >
        ${cancelText}
      </button>
    </div>
    <section style="margin-top: 7em;" class="c-marketing-block">
      <aside class="l-stack">
        <h3 class="t-title">${copy.what_is_phrase_q}</h3>
        <p class="t-paragraph">${copy.what_is_phrase_a}</p>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">${copy.how_save_phrase_q}</h3>
        <ul class="c-list c-list--bulleted">
          <li>${copy.how_save_phrase_a_1}</li>
          <li>${copy.how_save_phrase_a_2}</li>
          <li>${copy.how_save_phrase_a_3}</li>
        </ul>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">${copy.when_use_phrase_q}</h3>
        <p class="t-paragraph">${copy.when_use_phrase_a_1}</p>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">${copy.share_phrase_q}</h3>
        <p class="t-paragraph">${copy.share_phrase_a_1}</p>
      </aside>
    </section>
  `;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot,
  });
};

export const addPhrasePage = renderPage(addPhraseTemplate);

// Prompt the user to create a recovery phrase
export const addPhrase = ({
  cancelText,
}: {
  cancelText: string;
}): Promise<"ok" | "cancel"> => {
  return new Promise((resolve) =>
    addPhrasePage({
      i18n: new I18n(),
      ok: () => resolve("ok"),
      cancel: () => resolve("cancel"),
      cancelText,
      scrollToTop: true,
    })
  );
};

export const recoveryWizard = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  // Here, if the user doesn't have any recovery device, we prompt them to add
  // one.
  const recoveries = await withLoader(() =>
    connection.lookupRecovery(userNumber)
  );
  if (recoveries.length === 0) {
    const doAdd = await addPhrase({ cancelText: "Remind me later" });
    if (doAdd !== "cancel") {
      doAdd satisfies "ok";

      await setupRecovery({ userNumber, connection });
    }
  }
};
