import { displayError } from "$src/components/displayError";
import { mainWindow } from "$src/components/mainWindow";
import {
  apiResultToLoginFlowResult,
  cancel,
  LoginFlowCanceled,
  LoginFlowSuccess,
} from "$src/utils/flowResult";
import { Connection } from "$src/utils/iiConnection";
import { RecoveryDevice } from "$src/utils/recoveryDevice";
import { unreachable } from "$src/utils/utils";
import { html, render } from "lit-html";

const pageContent = () => {
  const pageContentSlot = html`
    <article>
      <hgroup>
        <h1 class="t-title t-title--main">Use Recovery Device</h1>
        <p class="t-lead">
          Click <strong class="t-strong">continue</strong> and follow your
          browser's instructions to recover your Internet Identity with external
          hardware.
        </p>
      </hgroup>
      <div class="c-button-group">
        <button
          id="recover-with-device__cancel"
          class="c-button c-button--secondary"
        >
          Cancel
        </button>
        <button
          id="recover-with-device__continue"
          class="c-button c-button--primary"
        >
          Continue
        </button>
      </div>
    </article>
  `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const deviceRecoveryPage = (
  userNumber: bigint,
  connection: Connection,
  device: RecoveryDevice
): Promise<LoginFlowSuccess | LoginFlowCanceled> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return init(userNumber, connection, device);
};

const init = (
  userNumber: bigint,
  connection: Connection,
  device: RecoveryDevice
): Promise<LoginFlowSuccess | LoginFlowCanceled> =>
  new Promise((resolve) => {
    const buttonContinue = document.getElementById(
      "recover-with-device__continue"
    ) as HTMLButtonElement | null;
    if (buttonContinue !== null) {
      buttonContinue.onclick = async () => {
        const { pubkey, credential_id } = device;

        // This is a sanity check to give a more precise error message in case the
        // inferred recovery device does not have webauthn credentials
        if (credential_id.length === 0) {
          await displayError({
            title: "No credentials found",
            message:
              "There were no credentials associated with the recovery device",
            primaryButton: "Try again",
          });
          return deviceRecoveryPage(userNumber, connection, device).then(
            (res) => resolve(res)
          );
        }
        const result = apiResultToLoginFlowResult(
          await connection.fromWebauthnCredentials(userNumber, [
            { credential_id: credential_id[0], pubkey },
          ])
        );

        switch (result.tag) {
          case "ok":
            resolve(result);
            break;
          case "err":
            await displayError({ ...result, primaryButton: "Try again" });
            void deviceRecoveryPage(userNumber, connection, device).then(
              (res) => resolve(res)
            );
            break;
          default:
            unreachable(result);
            break;
        }
      };
    }

    const buttonCancel = document.getElementById(
      "recover-with-device__cancel"
    ) as HTMLButtonElement | null;
    if (buttonCancel !== null) {
      buttonCancel.onclick = () => {
        resolve(cancel);
      };
    }
  });
