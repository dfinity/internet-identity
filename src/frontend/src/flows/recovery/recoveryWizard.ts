import { AuthenticatedConnection } from "../../utils/iiConnection";
import { setupRecovery } from "./setupRecovery";
import { displaySingleDeviceWarning } from "./displaySingleDeviceWarning";
import { displaySafariWarning } from "./displaySafariWarning";
import { iOSOrSafari } from "../../utils/utils";

export const recoveryWizard = async (
  connection: AuthenticatedConnection,
  userNumber: bigint
): Promise<void> =>
  iOSOrSafari()
    ? await recoveryWizardSafari(connection, userNumber)
    : await recoveryWizardDefault(connection, userNumber);

const recoveryWizardSafari = async (
  connection: AuthenticatedConnection,
  userNumber: bigint
): Promise<void> => {
  // Here, we let the user know about the quirks of Safari. The menu will take
  // them to the device picker, if they choose to.
  if ((await connection.lookupRecovery(userNumber)).length === 0) {
    await displaySafariWarning(connection, userNumber);
  }
};

const recoveryWizardDefault = async (
  connection: AuthenticatedConnection,
  userNumber: bigint
): Promise<void> => {
  // Here, if the user doesn't have any recovery device, we prompt them to add
  // one. If after returning from the prompt they still haven't added one, then
  // we display a big warning with full explanation about the risks of having a
  // single authentication device and not having a recovery device.
  if ((await connection.lookupRecovery(userNumber)).length === 0) {
    await setupRecovery(connection, userNumber);
    if ((await connection.lookupRecovery(userNumber)).length === 0) {
      await displaySingleDeviceWarning(connection, userNumber);
    }
  }
};
