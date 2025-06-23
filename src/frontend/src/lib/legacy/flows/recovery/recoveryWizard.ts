import { withLoader } from "$lib/templates/loader";
import { I18n } from "$lib/legacy/i18n";
import { renderPage } from "$lib/utils/lit-html";
import { TemplateResult } from "lit-html";

import { AuthenticatedConnection } from "$lib/utils/iiConnection";

import type { DeviceData } from "$lib/generated/internet_identity_types";
import { infoScreenTemplate } from "$lib/templates/infoScreen";
import { DOMAIN_COMPATIBILITY } from "$lib/state/featureFlags";
import { get } from "svelte/store";
import { IdentityMetadata } from "$lib/legacy/repositories/identityMetadata";
import { getCredentialsOrigin } from "$lib/utils/credential-devices";
import { userSupportsWebauthRoR } from "$lib/utils/rorSupport";
import { isNullish } from "@dfinity/utils";
import { addDevice } from "../addDevice/manage/addDevice";
import {
  PinIdentityMaterial,
  idbRetrievePinIdentityMaterial,
} from "../pin/idb";
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
    }),
  );
};

type DeviceStatus = "pin-only" | "one-device";

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
    "one-device": [
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

// Prompt the user to create a recovery phrase
export const addDeviceWarning = ({
  status,
}: {
  status: DeviceStatus;
}): Promise<{ action: "remind-later" | "do-not-remind" | "add-device" }> => {
  return new Promise((resolve) =>
    addDeviceWarningPage({
      i18n: new I18n(),
      ok: () => resolve({ action: "add-device" }),
      remindLater: () => resolve({ action: "remind-later" }),
      doNotRemindAgain: () => resolve({ action: "do-not-remind" }),
      status,
    }),
  );
};

/**
 * Helper to encapsulate the logic of when and which recovery warning page to show.
 *
 * Three conditions must be met for the warning page to be shown:
 * * Not having seen the recovery page in the last week
 *    (on registration, the user is not shown the page, but set it as seen to not bother during the onboarding)
 * * The user has at most one device.
 *    (a phrase and pin are not considered a device, only normal devices or recovery devices)
 * * The user has not disabled the warning.
 *    (users can choose to not see the warning again by clicking "do not remind" button)
 *
 * When the warning page is shown, two different messages could be displayed:
 * * User has only the pin authentication method.
 * * User has only one device.
 *
 * @param params {Object}
 * @param params.credentials {Omit<DeviceData, "alias">[]}
 * @param params.identityMetadata {IdentityMetadata | undefined}
 * @param params.pinIdentityMaterial {PinIdentityMaterial | undefined}
 * @param params.nowInMillis {number}
 * @returns {DeviceStatus | "no-warning"}
 */
// Exported for testing
export const getDevicesStatus = ({
  credentials,
  identityMetadata,
  pinIdentityMaterial,
  nowInMillis,
}: {
  credentials: Omit<DeviceData, "alias">[];
  identityMetadata: IdentityMetadata | undefined;
  pinIdentityMaterial: PinIdentityMaterial | undefined;
  nowInMillis: number;
}): DeviceStatus | "no-warning" => {
  const ONE_WEEK_MILLIS = 7 * 24 * 60 * 60 * 1000;
  const oneWeekAgoTimestamp = nowInMillis - ONE_WEEK_MILLIS;
  const hasNotSeenRecoveryPageLastWeek =
    (identityMetadata?.recoveryPageShownTimestampMillis ?? 0) <
    oneWeekAgoTimestamp;
  const showWarningPageEnabled = isNullish(
    identityMetadata?.doNotShowRecoveryPageRequestTimestampMillis,
  );
  const totalDevicesCount = credentials.filter(
    ({ key_type }) => !("seed_phrase" in key_type),
  ).length;
  if (
    totalDevicesCount <= 1 &&
    hasNotSeenRecoveryPageLastWeek &&
    showWarningPageEnabled
  ) {
    if (totalDevicesCount === 0 && !pinIdentityMaterial) {
      // This should never happen because it means that the user has no devices and no pin.
      // But we still handle it to avoid a crash assuming there was an error retrieving the pin material.
      return "pin-only";
    }
    return totalDevicesCount === 0 ? "pin-only" : "one-device";
  }
  return "no-warning";
};

// TODO: Add e2e test https://dfinity.atlassian.net/browse/GIX-2600
export const recoveryWizard = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
): Promise<void> => {
  // Here, if the user doesn't have any recovery device, we prompt them to add
  // one.
  const [credentials, identityMetadata, pinIdentityMaterial] = await withLoader(
    () =>
      Promise.all([
        connection.lookupAll(userNumber),
        connection.getIdentityMetadata(),
        idbRetrievePinIdentityMaterial({
          userNumber,
        }),
      ]),
  );
  const nowInMillis = Date.now();

  const devicesStatus = getDevicesStatus({
    credentials,
    identityMetadata,
    pinIdentityMaterial,
    nowInMillis,
  });

  const originNewDevice =
    userSupportsWebauthRoR() && get(DOMAIN_COMPATIBILITY)
      ? getCredentialsOrigin({
          credentials,
        })
      : undefined;

  if (devicesStatus !== "no-warning") {
    connection.updateIdentityMetadata({
      recoveryPageShownTimestampMillis: nowInMillis,
    });
    const userChoice = await addDeviceWarning({
      status: devicesStatus,
    });
    if (userChoice.action === "add-device") {
      await addDevice({
        userNumber,
        connection,
        origin: originNewDevice ?? window.origin,
      });
    }
    if (userChoice.action === "do-not-remind") {
      connection.updateIdentityMetadata({
        doNotShowRecoveryPageRequestTimestampMillis: nowInMillis,
      });
    }
    // Do nothing if `"remind-later"`.
  }
};
