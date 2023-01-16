import { html, render } from "lit-html";
import { displayError } from "../../../components/displayError";
import { unreachable } from "../../../utils/utils";
import {
  apiResultToLoginFlowResult,
  cancel,
  LoginFlowSuccess,
  LoginFlowCanceled,
} from "../../../utils/flowResult";
import { DeviceData } from "../../../../generated/internet_identity_types";
import { Connection } from "../../../utils/iiConnection";
import { mainWindow } from "../../../components/mainWindow";

const pageContent = (userNumber: bigint) => mainWindow({
  showLogo: false,
  showFooter: false,
  slot: html`
  <article>
    <hgroup>
      <h1 class="t-title t-title--main">Recovery for ${userNumber}</h1>
      <p class="t-lead">
        You are about to recover your anchor using a recovery device. Please
        click "continue" and then follow your browser's instructions to connect
        your device.
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
`});

export const deviceRecoveryPage = async (
  userNumber: bigint,
  connection: Connection,
  device: DeviceData
): Promise<LoginFlowSuccess | LoginFlowCanceled> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init(userNumber, connection, device);
};

const init = (
  userNumber: bigint,
  connection: Connection,
  device: DeviceData
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
            deviceRecoveryPage(userNumber, connection, device).then((res) =>
              resolve(res)
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
      buttonCancel.onclick = async () => {
        resolve(cancel);
      };
    }
  });
