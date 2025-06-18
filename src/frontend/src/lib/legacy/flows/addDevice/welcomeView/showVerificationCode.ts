import type {
  AddTentativeDeviceResponse,
  CredentialId,
} from "$lib/generated/internet_identity_types";
import { displayError } from "$lib/templates/displayError";
import { mainWindow } from "$lib/templates/mainWindow";
import { toast } from "$lib/templates/toast";
import { tentativeDeviceStepper } from "$lib/legacy/flows/addDevice/stepper";
import { setAnchorUsed } from "$lib/legacy/storage";
import { AsyncCountdown } from "$lib/utils/countdown";
import { Connection } from "$lib/utils/iiConnection";
import { renderPage } from "$lib/utils/lit-html";
import { waitFor, unknownToString } from "$lib/utils/utils";
import { nonNullish } from "@dfinity/utils";
import { html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";

type TentativeRegistrationInfo = Extract<
  AddTentativeDeviceResponse,
  { added_tentatively: Record<string, unknown> }
>["added_tentatively"];

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
  const pageContentSlot = html`<article>
    ${tentativeDeviceStepper({ step: "verify" })}
    <hgroup>
      <div class="c-card__label">
        <h2>Internet Identity ${userNumber}</h2>
      </div>
      <h1 class="t-title t-title--main">Verify New Passkey</h1>
      <p class="t-paragraph">Your new Passkey:</p>
      <output
        class="c-input c-input--stack c-input--fullwidth c-input--readonly t-vip t-vip--small"
        aria-label="Passkey Alias"
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
      class="c-input c-input--stack c-input--fullwidth c-input--readonly t-vip"
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
    </div>
  </article>`;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: pageContentSlot,
  });
};

export const showVerificationCodePage = renderPage(
  showVerificationCodeTemplate,
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
  credentialToBeVerified: CredentialId,
): Promise<"ok" | "canceled"> => {
  const countdown: AsyncCountdown<"match" | "canceled"> =
    AsyncCountdown.fromNanos(
      tentativeRegistrationInfo.device_registration_timeout,
    );

  showVerificationCodePage({
    userNumber,
    alias,
    tentativeRegistrationInfo,
    remaining: countdown.remainingFormattedAsync(),
    cancel: () => {
      countdown.stop("canceled");
      return "canceled";
    },
  });

  // Poll repeatedly
  void (async () => {
    const result = await poll({
      userNumber,
      connection,
      credentialToBeVerified,
      shouldStop: () => countdown.hasStopped(),
    });
    countdown.stop(result);
  })();

  return await handlePollResult({ userNumber, result: await countdown.wait() });
};

// Poll until the user number (anchor) contains at least one device with
// credential id equal to 'credentialToBeVerified', or until `shouldStop`
// returns `true` for the first time.
const poll = ({
  userNumber,
  connection,
  credentialToBeVerified,
  shouldStop,
}: {
  userNumber: bigint;
  connection: Connection;
  credentialToBeVerified: CredentialId;
  shouldStop: () => boolean;
}): Promise<"match"> =>
  // eslint-disable-next-line no-async-promise-executor
  new Promise(async (resolve) => {
    const verifyCredentials = async (): Promise<boolean> => {
      try {
        return await anchorHasCredentials({
          userNumber,
          credential: credentialToBeVerified,
          connection,
        });
      } catch (e) {
        toast.error(
          "An error occurred while polling for verification: " +
            unknownToString(e, "no details available :("),
        );
        console.warn("Error occurred when polling:", e);
      }

      return false;
    };

    while (!shouldStop()) {
      if (await verifyCredentials()) {
        resolve("match");
        return;
      }

      // Debounce a little; in practice won't be noticed by users but
      // will avoid hot looping in case the credential verification becomes near instantaneous.
      await waitFor(100);
    }
  });

const handlePollResult = async ({
  userNumber,
  result,
}: {
  userNumber: bigint;
  result: "match" | "canceled" | typeof AsyncCountdown.timeout;
}): Promise<"ok" | "canceled"> => {
  if (result === "match") {
    await setAnchorUsed(userNumber);
    return "ok";
  } else if (result === AsyncCountdown.timeout) {
    await displayError({
      title: "Timeout Reached",
      message:
        'The timeout has been reached. For security reasons the "add device" process has been aborted.',
      primaryButton: "Ok",
    });
    return "canceled";
  } else {
    result satisfies "canceled";
    return "canceled";
  }
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
      credentialIdEqual(device.credential_id[0], credential),
  );
  return nonNullish(matching);
};

function credentialIdEqual(
  credentialId1: CredentialId,
  credentialId2: CredentialId,
): boolean {
  if (credentialId1.length !== credentialId2.length) {
    return false;
  }
  return credentialId1.every((elem, index) => elem === credentialId2[index]);
}
