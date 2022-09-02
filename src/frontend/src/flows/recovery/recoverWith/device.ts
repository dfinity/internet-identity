import { html, render } from "lit-html";
import { displayError } from "../../../components/displayError";
import { unreachable } from "../../../utils/utils";
import {
  apiResultToLoginFlowResult,
  canceled,
  LoginFlowSuccess,
  LoginFlowCanceled,
} from "../../login/flowResult";
import { DeviceData } from "../../../../generated/internet_identity_types";
import { Connection } from "../../../utils/iiConnection";
import { startCardAnimation } from "../../../utils/animation";

// TODO: not in the showcase

const pageContent = (userNumber: bigint) => html`
  <style>
    .full-width {
      width: 100%;
    }
  </style>
  <div class="l-container c-card c-card--bg full-width">
    <div class="c-card-bg">
      <canvas class="c-card-bg__canvas" width="32" height="32"></canvas>
    </div>
    <h1>Recovery for ${userNumber}</h1>
    <p>
      You are about to recover your anchor using a recovery device. Please click
      "continue" and then follow your browser's instructions to connect your
      device.
    </p>
    <button
      id="recover-with-device__continue"
      class="c-button c-button--primary"
    >
      Continue
    </button>
    <button
      id="recover-with-device__cancel"
      class="c-button c-button--secondary"
    >
      Cancel
    </button>
  </div>
`;

export const deviceRecoveryPage = async (
  userNumber: bigint,
  connection: Connection,
  device: DeviceData
): Promise<LoginFlowSuccess | LoginFlowCanceled> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  startCardAnimation();
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
        resolve(canceled());
      };
    }
  });
