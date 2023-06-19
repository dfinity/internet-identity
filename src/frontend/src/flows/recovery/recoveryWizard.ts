import { withLoader } from "$src/components/loader";
import { I18n } from "$src/i18n";
import { renderPage } from "$src/utils/lit-html";
import { TemplateResult } from "lit-html";

import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { setupRecovery } from "./setupRecovery";

import { infoScreenTemplate } from "$src/components/infoScreen";
import copyJson from "./recoveryWizard.json";

/* Phrase creation kick-off screen */

const addPhraseTemplate = ({
  ok,
  cancel,
  i18n,
  scrollToTop = false,
  intent,
}: {
  ok: () => void;
  cancel: () => void;
  i18n: I18n;
  /* put the page into view */
  scrollToTop?: boolean;
  /* Whether shown after the user initiated a phrase creation (info) or as a security reminder
   * (more like a warning) */
  intent: "userInitiated" | "securityReminder";
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const [cancelText, icon, label] = {
    userInitiated: [copy.cancel, "info", copy.label_info] as const,
    securityReminder: [copy.skip, "warning", copy.label_warning] as const,
  }[intent];

  return infoScreenTemplate({
    cancel,
    cancelText,
    next: ok,
    nextText: copy.ok,
    title: copy.title,
    paragraph: copy.paragraph,
    scrollToTop,
    icon,
    pageId: "add-recovery-phrase",
    label,
    entries: [
      { header: copy.what_is_phrase_q, content: copy.what_is_phrase_a },
      {
        header: copy.how_save_phrase_q,
        content: [
          copy.how_save_phrase_a_1,
          copy.how_save_phrase_a_2,
          copy.how_save_phrase_a_3,
        ],
      },
      { header: copy.when_use_phrase_q, content: copy.when_use_phrase_a_1 },
      { header: copy.share_phrase_q, content: copy.share_phrase_a_1 },
    ],
  });
};

export const addPhrasePage = renderPage(addPhraseTemplate);

// Prompt the user to create a recovery phrase
export const addPhrase = ({
  intent,
}: {
  intent: Parameters<typeof addPhrasePage>[0]["intent"];
}): Promise<"ok" | "cancel"> => {
  return new Promise((resolve) =>
    addPhrasePage({
      i18n: new I18n(),
      ok: () => resolve("ok"),
      cancel: () => resolve("cancel"),
      scrollToTop: true,
      intent,
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
    const doAdd = await addPhrase({ intent: "securityReminder" });
    if (doAdd !== "cancel") {
      doAdd satisfies "ok";

      await setupRecovery({ userNumber, connection });
    }
  }
};
