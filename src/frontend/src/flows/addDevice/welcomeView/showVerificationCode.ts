import { html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import {
  AddTentativeDeviceResponse,
  CredentialId,
} from "../../../../generated/internet_identity_types";
import { displayError } from "../../../components/displayError";
import { mainWindow } from "../../../components/mainWindow";
import { AsyncCountdown } from "../../../utils/countdown";
import { Connection } from "../../../utils/iiConnection";
import { renderPage } from "../../../utils/lit-html";
import { setAnchorUsed } from "../../../utils/userNumber";
import { delayMillis, unreachableLax } from "../../../utils/utils";

type TentativeRegistrationInfo = Extract<
  AddTentativeDeviceResponse,
  { added_tentatively: Record<string, unknown> }
>["added_tentatively"];

const showVerificationCodeTemplate = ({
  alias,
  tentativeRegistrationInfo,
  remaining,
  cancel,
}: {
  alias: string;
  tentativeRegistrationInfo: TentativeRegistrationInfo;
  remaining: AsyncIterable<string>;
  cancel: () => void;
}) => {
  const pageContentSlot = html` <hgroup>
      <h1 class="t-title t-title--main">Verify New Device</h1>
      <p class="t-paragraph">Your new device:</p>
      <output
        class="c-input c-input--readonly t-vip t-vip--small"
        aria-label="Device Alias"
        >${alias}</output
      >
      <p class="t-paragraph">
        In your other Internet Identity window, confirm that you trust this
        device by entering the
        <strong class="t-strong">Verification Code</strong>:
      </p>
    </hgroup>
    <output
      id="verificationCode"
      class="c-input c-input--readonly t-vip"
      aria-label="Verification Code"
    >
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

export const showVerificationCodePage = renderPage(
  showVerificationCodeTemplate
);

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
): Promise<"ok"> => {
  const countdown = AsyncCountdown.fromNanos(
    tentativeRegistrationInfo.device_registration_timeout
  );

  showVerificationCodePage({
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
  credentialToBeVerified: CredentialId;
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
}): Promise<"ok"> => {
  if (pollResult === "match") {
    setAnchorUsed(userNumber);
    return "ok";
  } else if (pollResult === "timeout") {
    await displayError({
      title: "Timeout Reached",
      message:
        'The timeout has been reached. For security reasons the "add device" process has been aborted.',
      primaryButton: "Ok",
    });
    return window.location.reload as never;
  }

  unreachableLax(pollResult);
  return window.location.reload as never;
};

// Returns true if the given anchor has credentials with the given credential id.
const anchorHasCredentials = async ({
  userNumber,
  credential,
  connection,
}: {
  userNumber: bigint;
  credential: CredentialId;
  connection: Connection;
}) => {
  const devices = await connection.lookupAuthenticators(userNumber);
  const matching = devices.find(
    (device) =>
      device.credential_id.length === 1 &&
      credentialIdEqual(device.credential_id[0], credential)
  );
  return matching !== undefined;
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
