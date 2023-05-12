import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { html } from "lit-html";
import { setupRecovery } from "./setupRecovery";

export const recoveryWizard = async (
  userNumber: bigint,
  connection: AuthenticatedConnection
): Promise<void> => {
  // Here, if the user doesn't have any recovery device, we prompt them to add
  // one.
  if ((await connection.lookupRecovery(userNumber)).length === 0) {
    await setupRecoveryWizard(userNumber, connection);
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
