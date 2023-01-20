import { html, render } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { Connection } from "../../../utils/iiConnection";
import { delayMillis } from "../../../utils/utils";
import {
  CredentialId,
  Timestamp,
} from "../../../../generated/internet_identity_types";
import { setAnchorUsed } from "../../../utils/userNumber";
import { AsyncCountdown } from "../../../utils/countdown";
import { displayError } from "../../../components/displayError";
import { mainWindow } from "../../../components/mainWindow";

export type TentativeRegistrationInfo = {
  verification_code: string;
  device_registration_timeout: Timestamp;
};

const showVerificationCodeTemplate = ({
  userNumber,
  alias,
  tentativeRegistrationInfo,
  remaining,
  cancel,
}: {
  userNumber: bigint;
  alias: string;
  tentativeRegistrationInfo: TentativeRegistrationInfo;
  remaining: AsyncIterable<string>;
  cancel: () => void;
}) => {
  const pageContentSlot = html` <hgroup>
      <h1 class="t-title t-title--main">Device Verification Required</h1>
      <p class="t-lead">
        This device was added tentatively to the Identity Anchor
        <strong>${userNumber}</strong>. Log in on an existing device and verify
        this device using the verification code below. After successful
        verification this page will automatically refresh.
      </p>
    </hgroup>
    <h2 class="t-title">Alias</h2>
    <output class="c-input c-input--readonly t-vip t-vip--small"
      >${alias}</output
    >
    <h2 class="t-title">Device Verification Code</h2>
    <output id="verificationCode" class="c-input c-input--readonly t-vip">
      ${tentativeRegistrationInfo.verification_code}
    </output>
    <div class="l-stack">
      <p>Time remaining: <span id="timer">${asyncReplace(remaining)}</span></p>
      <div class="l-stack">
        <button @click=${() => cancel()} class="c-button c-button--secondary">
          Cancel
        </button>
      </div>
    </div>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const showVerificationCodePage = (
  props: Parameters<typeof showVerificationCodeTemplate>[0],
  container?: HTMLElement
): void => {
  const contain =
    container ?? (document.getElementById("pageContent") as HTMLElement);
  render(showVerificationCodeTemplate(props), contain);
};

/**
 * Page to display the verification code which is received after successfully registering a tentative device.
 * @param userNumber Anchor the device to be verified belongs to
 * @param alias Alias of the tentative device
 * @param tentativeRegistrationInfo Verification code and timeout received when registering the tentative device
 * @param credentialToBeVerified Credential id of the device to be verified. When this id appears in the list of authenticators, verification was successful.
 */
export const showVerificationCode = async (
  userNumber: bigint,
  connection: Connection,
  alias: string,
  tentativeRegistrationInfo: TentativeRegistrationInfo,
  credentialToBeVerified: CredentialId
): Promise<void> => {
  const countdown = AsyncCountdown.fromNanos(
    tentativeRegistrationInfo.device_registration_timeout
  );

  showVerificationCodePage({
    userNumber,
    alias,
    tentativeRegistrationInfo,
    remaining: countdown.remainingFormattedAsync(),
    cancel: () => {
      countdown.stop();
      window.location.reload();
    },
  });

  const pollResult = await poll({
    userNumber,
    connection,
    credentialToBeVerified,
    shouldStop: () => countdown.hasStopped(),
  });

  return await handlePollResult({ userNumber, pollResult });
};

// Poll until the user number (anchor) contains at least one device with
// credential id equal to 'credentialToBeVerified', or until `shouldStop`
// returns `true` for the first time.
async function poll({
  userNumber,
  connection,
  credentialToBeVerified,
  shouldStop,
}: {
  userNumber: bigint;
  connection: Connection;
  credentialToBeVerified: Array<number>;
  shouldStop: () => boolean;
}): Promise<"timeout" | "match"> {
  const verifyCredentials = async (): Promise<boolean> => {
    let res = undefined;
    try {
      res = await anchorHasCredentials({
        userNumber,
        credential: credentialToBeVerified,
        connection,
      });
    } catch (e) {
      // Silently discard the error (though log in console) until we have
      // support for e.g. toasts
      console.warn("Error occurred when polling:", e);
    }

    return res ?? false;
  };

  while (!shouldStop()) {
    if (await verifyCredentials()) {
      return "match";
    }

    // Debounce a little; in practice won't be noticed by users but
    // will avoid hot looping in case the credential verification becomes near instantaneous.
    await delayMillis(100);
  }

  return "timeout";
}

const handlePollResult = async ({
  userNumber,
  pollResult,
}: {
  userNumber: bigint;
  pollResult: "match" | "timeout";
}) => {
  if (pollResult === "match") {
    setAnchorUsed(userNumber);
    window.location.reload();
  } else if (pollResult === "timeout") {
    await displayError({
      title: "Timeout Reached",
      message:
        'The timeout has been reached. For security reasons the "add device" process has been aborted.',
      primaryButton: "Ok",
    });
    window.location.reload();
  }
};

// Returns true if the given anchor has credentials with the given credential id.
const anchorHasCredentials = async ({
  userNumber,
  credential,
  connection,
}: {
  userNumber: bigint;
  credential: Array<number>;
  connection: Connection;
}) => {
  const devices = await connection.lookupAuthenticators(userNumber);
  for (const device of devices) {
    if (
      device.credential_id.length === 1 &&
      credentialIdEqual(device.credential_id[0], credential)
    ) {
      return true;
    }
  }

  return false;
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
