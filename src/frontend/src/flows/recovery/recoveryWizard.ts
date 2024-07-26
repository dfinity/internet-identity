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

type DeviceStatus = "pin-only" | "one-passkey";

const addDeviceWarningTemplate = ({
  ok,
  remindLater,
  doNotRemindAgain,
  i18n,
  status,
}: {
  ok: () => void;
  remindLater: () => void;
  doNotRemindAgain: () => void;
  i18n: I18n;
  status: DeviceStatus;
}): TemplateResult => {
  const copy = i18n.i18n(copyJson);

  const [paragraph, title] = {
    "pin-only": [
      copy.paragraph_add_device_pin_only,
      copy.add_device_title_pin_only,
    ],
    "one-passkey": [
      copy.paragraph_add_device_one_passkey,
      copy.add_device_title_one_passkey,
    ],
  }[status];

  return infoScreenTemplate({
    cancel: remindLater,
    cancelText: copy.remind_later,
    next: ok,
    nextText: copy.add_device,
    additionalAction: doNotRemindAgain,
    additionalActionText: copy.do_not_remind,
    title,
    paragraph,
    scrollToTop: true,
    icon: "warning",
    pageId: "add-another-device-warning",
    label: copy.label_warning,
  });
};

// TODO: Create the `addDeviceWarning` page and use it in `recoveryWizard` function.
export const addDeviceWarningPage = renderPage(addDeviceWarningTemplate);

// TODO: Add e2e test https://dfinity.atlassian.net/browse/GIX-2600
export const recoveryWizard = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  // Here, if the user doesn't have any recovery device, we prompt them to add
  // one.
  const [recoveries, identityMetadata] = await withLoader(() =>
    Promise.all([
      connection.lookupRecovery(userNumber),
      connection.getIdentityMetadata(),
    ])
  );

  const ONE_WEEK_MILLIS = 7 * 24 * 60 * 60 * 1000;
  const nowInMillis = Date.now();
  const oneWeekAgoTimestamp = nowInMillis - ONE_WEEK_MILLIS;
  const hasNotSeenRecoveryPageLastWeek =
    (identityMetadata?.recoveryPageShownTimestampMillis ?? 0) <
    oneWeekAgoTimestamp;
  if (recoveries.length === 0 && hasNotSeenRecoveryPageLastWeek) {
    // `await` here doesn't add any waiting time beacause we already got the metadata earlier.
    await connection.updateIdentityMetadata({
      recoveryPageShownTimestampMillis: nowInMillis,
    });
    const doAdd = await addPhrase({ intent: "securityReminder" });
    if (doAdd !== "cancel") {
      doAdd satisfies "ok";

      await setupRecovery({ userNumber, connection });
    }
  }
};
