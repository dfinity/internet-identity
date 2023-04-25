import { displayError } from "$root/components/displayError";
import { mainWindow } from "$root/components/mainWindow";
import {
  apiResultToLoginFlowResult,
  cancel,
  LoginFlowCanceled,
  LoginFlowSuccess,
} from "$root/utils/flowResult";
import { Connection } from "$root/utils/iiConnection";
import { RecoveryDevice } from "$root/utils/recoveryDevice";
import { unreachable } from "$root/utils/utils";
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
        const result = apiResultToLoginFlowResult(
          await connection.fromWebauthnDevices(userNumber, [device])
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
