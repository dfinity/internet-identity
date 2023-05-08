import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { iOSOrSafari } from "$src/utils/utils";
import { html } from "lit-html";
import { displaySafariWarning } from "./displaySafariWarning";
import { displaySingleDeviceWarning } from "./displaySingleDeviceWarning";
import { setupRecovery } from "./setupRecovery";

export const recoveryWizard = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> =>
  iOSOrSafari()
    ? await recoveryWizardSafari(userNumber, connection)
    : await recoveryWizardDefault(userNumber, connection);

const recoveryWizardSafari = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  // Here, we let the user know about the quirks of Safari. The menu will take
  // them to the device picker, if they choose to.
  if ((await connection.lookupRecovery(userNumber)).length === 0) {
    await displaySafariWarning(userNumber, connection, setupRecoveryWizard);
  }
};

const recoveryWizardDefault = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  // Here, if the user doesn't have any recovery device, we prompt them to add
  // one. If after returning from the prompt they still haven't added one, then
  // we display a big warning with full explanation about the risks of having a
  // single authentication device and not having a recovery device.
  if ((await connection.lookupRecovery(userNumber)).length === 0) {
    await setupRecoveryWizard(userNumber, connection);
    if ((await connection.lookupRecovery(userNumber)).length === 0) {
      await displaySingleDeviceWarning(
        userNumber,
        connection,
        setupRecoveryWizard
      );
    }
  }
};

const setupRecoveryWizard = (
  userNumber: bigint,
  connection: AuthenticatedConnection
) =>
  setupRecovery({
    userNumber,
    connection,
    title: html`Choose a Recovery Method`,

    message: html`We recommend that you create at least one recovery method in
    case you lose your Passkeys.`,
    cancelText: html`Skip, I understand the risks`,
  });
