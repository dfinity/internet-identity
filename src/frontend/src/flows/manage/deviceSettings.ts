import { DeviceData } from "$generated/internet_identity_types";
import { promptDeviceAlias } from "$src/components/alias";
import { displayError } from "$src/components/displayError";
import { infoScreenTemplate } from "$src/components/infoScreen";
import { withLoader } from "$src/components/loader";
import { recoverWithPhrase } from "$src/flows/recovery/recoverWith/phrase";
import { phraseWizard } from "$src/flows/recovery/setupRecovery";
import { I18n } from "$src/i18n";
import {
  AuthenticatedConnection,
  bufferEqual,
  Connection,
} from "$src/utils/iiConnection";
import { renderPage } from "$src/utils/lit-html";
import {
  isProtected,
  isRecoveryDevice,
  RecoveryPhrase,
} from "$src/utils/recoveryDevice";
import { unknownToString, unreachable } from "$src/utils/utils";
import { DerEncodedPublicKey } from "@dfinity/agent";

import copyJson from "./deviceSettings.json";

/* Rename the device and return */
export const renameDevice = async ({
  connection,
  device,
  reload,
}: {
  connection: AuthenticatedConnection;
  device: DeviceData;
  reload: () => void;
}) => {
  const alias = await promptDeviceAlias({
    title: "Passkey Nickname",
    message: "Choose a nickname for this Passkey",
    value: device.alias,
  });
  if (alias === null) {
    reload();
    return;
  }

  device.alias = alias;

  await withLoader(async () => {
    await connection.update(device);
  });
  reload();
  return;
};
/* Remove the device and return */
export const deleteDevice = async ({
  connection,
  device,
  reload,
}: {
  connection: AuthenticatedConnection;
  device: DeviceData;
  reload: () => void;
}) => {
  const pubKey: DerEncodedPublicKey = new Uint8Array(device.pubkey)
    .buffer as DerEncodedPublicKey;
  const sameDevice = bufferEqual(
    connection.identity.getPublicKey().toDer(),
    pubKey
  );

  // Different confirmation based on the device
  const confirmationPrompt = [];
  if (isRecoveryDevice(device)) {
    confirmationPrompt.push("Remove your Recovery Device");
    confirmationPrompt.push(
      "Are you sure you want to remove your recovery device? You will no longer be able to use it to recover your account."
    );
  } else {
    confirmationPrompt.push(
      `Do you really want to remove the device "${device.alias}"?`
    );
  }
  if (sameDevice) {
    confirmationPrompt.push(
      "This will remove your current device and you will be logged out."
    );
  }
  const shouldProceed = confirm(confirmationPrompt.join("\n\n"));
  if (!shouldProceed) {
    return;
  }

  await withLoader(async () => {
    await connection.remove(device.pubkey);
  });

  if (sameDevice) {
    // clear anchor and reload the page.
    // do not call "reload", otherwise the management page will try to reload the list of devices which will cause an error
    localStorage.clear();
    location.reload();
    return;
  } else {
    reload();
  }
};

/* Resetting */

export const resetPhraseInfoTemplate = ({
  next,
  cancel,
  i18n,
  scrollToTop = false,
}: {
  next: () => void;
  cancel: () => void;
  i18n: I18n;
  /* put the page into view */
  scrollToTop?: boolean;
}) => {
  const copy = i18n.i18n(copyJson);
  return infoScreenTemplate({
    next,
    nextText: copy.reset_next,
    cancel,
    cancelText: copy.reset_cancel,
    title: copy.reset_title,
    paragraph: copy.reset_paragraph,
    entries: [
      {
        header: copy.reset_what_happens_q,
        content: copy.reset_what_happens_a,
      },
      {
        header: copy.reset_save_q,
        content: [
          copy.reset_save_a_1,
          copy.reset_save_a_2,
          copy.reset_save_a_3,
        ],
      },
      { header: copy.reset_use_q, content: copy.reset_use_a },
      { header: copy.reset_share_q, content: copy.reset_share_a },
    ],
    pageId: "reset-phrase-info",
    label: copy.reset_label,
    icon: "warning",
    scrollToTop,
  });
};

export const resetPhraseInfoPage = renderPage(resetPhraseInfoTemplate);

const resetPhraseInfo = (): Promise<"ok" | "cancel"> => {
  return new Promise((resolve) => {
    resetPhraseInfoPage({
      i18n: new I18n(),
      next: () => resolve("ok"),
      cancel: () => resolve("cancel"),
    });
  });
};

/* Reset the device and return to caller */
export const resetPhrase = async ({
  userNumber,
  connection,
  device,
  reload,
}: {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  device: DeviceData & RecoveryPhrase;
  reload: (connection?: AuthenticatedConnection) => void;
}): Promise<void> => {
  const confirmed = await resetPhraseInfo();
  if (confirmed === "cancel") {
    reload();
    return;
  }

  confirmed satisfies "ok";

  const sameDevice = bufferEqual(
    connection.identity.getPublicKey().toDer(),
    new Uint8Array(device.pubkey).buffer as DerEncodedPublicKey
  );

  // The connection used in the replace op
  // (if the phrase is protected, this prompts for the phrase and builds a new connection)
  const opConnection = isProtected(device)
    ? await deviceConnection(
        connection,
        "Please input your recovery phrase to reset it."
      )
    : connection;
  if (opConnection === null) {
    // User aborted
    return reload();
  }

  const uploadPhrase = (pubkey: DerEncodedPublicKey): Promise<void> =>
    withLoader(() =>
      opConnection.replace(device.pubkey, {
        ...device,
        pubkey: Array.from(new Uint8Array(pubkey)),
      })
    );

  const res = await phraseWizard({
    userNumber,
    operation: "reset",
    uploadPhrase,
  });

  if ("ok" in res) {
    // If the user was authenticated with the phrase, then replace the connection
    // to use the new phrase to void logging them out
    let nextConnection = undefined;
    if (sameDevice) {
      nextConnection = (await connection.fromIdentity(userNumber, res.ok))
        .connection;
    }
    return reload(nextConnection);
  } else if ("error" in res) {
    await displayError({
      title: "Could not reset recovery phrase",
      message: "An unexpected error occurred",
      detail: unknownToString(res.error, "unknown error"),
      primaryButton: "Ok",
    });
    return reload();
  } else {
    res satisfies { canceled: void };
    return reload();
  }
};

/* Protecting */

export const protectDeviceInfoTemplate = ({
  next,
  cancel,
  i18n,
  scrollToTop = false,
}: {
  next: () => void;
  cancel: () => void;
  i18n: I18n;
  scrollToTop?: boolean;
}) => {
  const copy = i18n.i18n(copyJson);
  return infoScreenTemplate({
    next,
    nextText: copy.protect_next,
    cancel,
    cancelText: copy.protect_cancel,
    title: copy.protect_title,
    paragraph: copy.protect_paragraph,
    entries: [
      {
        header: copy.protect_what_happens_q,
        content: copy.protect_what_happens_a,
      },
      {
        header: copy.protect_save_q,
        content: [
          copy.protect_save_a_1,
          copy.protect_save_a_2,
          copy.protect_save_a_3,
        ],
      },
      { header: copy.protect_use_q, content: copy.protect_use_a },
      { header: copy.protect_share_q, content: copy.protect_share_a },
    ],
    pageId: "protect-phrase-info",
    label: copy.protect_label,
    icon: "warning",
    scrollToTop,
  });
};

export const protectDeviceInfoPage = renderPage(protectDeviceInfoTemplate);

const protectDeviceInfo = (): Promise<"ok" | "cancel"> => {
  return new Promise((resolve) => {
    protectDeviceInfoPage({
      i18n: new I18n(),
      next: () => resolve("ok"),
      cancel: () => resolve("cancel"),
    });
  });
};

/* Protect the device and re-render the device settings (with the updated device) */
export const protectDevice = async ({
  connection,
  device,
  reload,
}: {
  connection: AuthenticatedConnection;
  device: DeviceData & RecoveryPhrase;
  reload: () => void;
}) => {
  const confirmed = await protectDeviceInfo();
  if (confirmed === "cancel") {
    reload();
    return;
  }

  confirmed satisfies "ok";

  device.protection = { protected: null };

  // NOTE: we do _not_ need to be authenticated with the device in order to protect it,
  // but we do it to make sure one last time that the user can actually successfully authenticate
  // with the device.
  const newConnection = await deviceConnection(
    connection,
    "Please input your recovery phrase to lock it."
  );

  // if null then user canceled so we just redraw the manage page
  if (newConnection == null) {
    await reload();
    return;
  }

  await withLoader(async () => {
    await newConnection.update(device);
  });
  reload();
};

/* Unprotect */

export const unprotectDeviceInfoTemplate = ({
  next,
  cancel,
  i18n,
  scrollToTop = false,
}: {
  next: () => void;
  cancel: () => void;
  i18n: I18n;
  /* put the page into view */
  scrollToTop?: boolean;
}) => {
  const copy = i18n.i18n(copyJson);
  return infoScreenTemplate({
    next,
    nextText: copy.unprotect_next,
    cancel,
    cancelText: copy.unprotect_cancel,
    title: copy.unprotect_title,
    paragraph: copy.unprotect_paragraph,
    entries: [],
    pageId: "unprotect-phrase-info",
    label: copy.unprotect_label,
    icon: "warning",
    scrollToTop,
  });
};

export const unprotectDeviceInfoPage = renderPage(unprotectDeviceInfoTemplate);

const unprotectDeviceInfo = (): Promise<"ok" | "cancel"> => {
  return new Promise((resolve) => {
    unprotectDeviceInfoPage({
      i18n: new I18n(),
      next: () => resolve("ok"),
      cancel: () => resolve("cancel"),
    });
  });
};

/* Unprotect the device and re-render the device settings (with the updated device) */
export const unprotectDevice = async (
  connection: AuthenticatedConnection,
  device: DeviceData & RecoveryPhrase,
  back: () => void
) => {
  const confirmed = await unprotectDeviceInfo();
  if (confirmed === "cancel") {
    back();
    return;
  }

  device.protection = { unprotected: null };

  // NOTE: we do need to be authenticated with the device in order to unprotect it
  const newConnection = await deviceConnection(
    connection,
    "Please input your recovery phrase to unlock it."
  );

  await withLoader(async () => {
    // if null then user canceled so we just redraw the manage page
    if (newConnection == null) {
      await back();
      return;
    }

    await newConnection.update(device);
  });
  back();
};

// Get a connection that's authenticated with the given device
// NOTE: this expects a recovery phrase device
const deviceConnection = async (
  connection: Connection,
  recoveryPhraseMessage: string
): Promise<AuthenticatedConnection | null> => {
  try {
    const loginResult = await recoverWithPhrase({
      connection,
      message: recoveryPhraseMessage,
    });
    switch (loginResult.tag) {
      case "ok":
        return loginResult.connection;
      case "canceled":
        return null;
      default:
        unreachable(loginResult);
        break;
    }
  } catch (error: unknown) {
    await displayError({
      title: "Could not modify device",
      message:
        "An unexpected error occurred when trying to read recovery phrase for device modification.",
      detail: error instanceof Error ? error.toString() : "unknown error",
      primaryButton: "Ok",
    });
    return null;
  }
};
