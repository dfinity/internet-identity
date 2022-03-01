import { html, render } from "lit-html";
import { IIConnection } from "../../utils/iiConnection";
import {
  CredentialId,
  Timestamp,
} from "../../../generated/internet_identity_types";
import { setUserNumber } from "../../utils/userNumber";
import { Principal } from "@dfinity/principal";
import { formatRemainingTime, setupCountdown } from "../../utils/countdown";

export type TentativeRegistrationInfo = {
  pin: string;
  device_registration_timeout: Timestamp;
};
const pageContent = (
  userNumber: bigint,
  alias: string,
  publicKey: string,
  tentativeRegistrationInfo: TentativeRegistrationInfo
) => html`
  <div class="container">
    <h1>Device Added Tentatively</h1>
    <p>
      This device was added tentatively to the Identity Anchor ${userNumber}.
      Log in on an existing device and verify this device using the PIN below.
      The page will automatically refresh when this device was verified.
    </p>
    <label>Alias</label>
    <div class="highlightBox">${alias}</div>
    <label>Public Key</label>
    <div class="highlightBox">${publicKey}</div>
    <label>Device Verification PIN</label>
    <div class="highlightBox">${tentativeRegistrationInfo.pin}</div>
    <p>
      Time remaining:
      <span id="timer"
        >${formatRemainingTime(
          tentativeRegistrationInfo.device_registration_timeout
        )}</span
      >
    </p>
    <button id="showPinCancel">Cancel</button>
  </div>
`;
export const showPin = async (
  userNumber: bigint,
  alias: string,
  principal: Principal,
  tentativeRegistrationInfo: TentativeRegistrationInfo,
  credentialToBeVerified: CredentialId
): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(
    pageContent(
      userNumber,
      alias,
      principal.toString(),
      tentativeRegistrationInfo
    ),
    container
  );
  return init(
    userNumber,
    tentativeRegistrationInfo.device_registration_timeout,
    credentialToBeVerified
  );
};

const init = async (
  userNumber: bigint,
  endTimestamp: bigint,
  credentialToBeVerified: CredentialId
): Promise<void> => {
  const countdown = setupCountdown(endTimestamp, async () => {
    window.location.reload();
  });
  const pollingHandler = window.setInterval(async () => {
    const deviceData = await IIConnection.lookupAuthenticators(userNumber);
    deviceData.forEach((device) => {
      if (device.credential_id.length !== 1) {
        console.log("invalid credential id");
        return;
      }
      const credentialId = device.credential_id[0];
      if (credentialIdEqual(credentialId, credentialToBeVerified)) {
        setUserNumber(userNumber);
        countdown.stop();
        window.clearInterval(pollingHandler);
        // TODO L2-309: do this without reload
        window.location.reload();
      }
    });
  }, 2000);

  const cancelButton = document.getElementById(
    "showPinCancel"
  ) as HTMLButtonElement;

  cancelButton.onclick = () => {
    countdown.stop();
    window.clearInterval(pollingHandler);
    // TODO L2-309: do this without reload
    window.location.reload();
  };
};

function credentialIdEqual(
  credentialId1: CredentialId,
  credentialId2: CredentialId
): boolean {
  if (credentialId1.length !== credentialId2.length) {
    return false;
  }
  return credentialId1.every((elem, index) => elem === credentialId2[index]);
}
