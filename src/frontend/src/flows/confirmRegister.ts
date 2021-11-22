import { html, render } from "lit-html";
import { displayUserNumber } from "./displayUserNumber";
import { setUserNumber } from "../utils/userNumber";
import { apiResultToLoginResult, LoginResult } from "./loginUnknown";
import { ProofOfWork } from "../../generated/internet_identity_types";
import { WebAuthnIdentity } from "@dfinity/identity";
import getProofOfWork from "../crypto/pow";
import { Principal } from "@dfinity/principal";
import { withLoader } from "../components/loader";
import {
  IIConnection,
  canisterIdPrincipal,
  ChallengeResult,
} from "../utils/iiConnection";

const pageContent = html`
  <div class="container">
    <h1>Confirm new device</h1>
    <form id="confirmForm">
      <p class="loading-captcha-text">Loading captcha...</p>
      <img id="captchaImg" />
      <input id="captchaInput" />
      <p>Please confirm to add your device.</p>
      <button type="submit" class="primary" id="confirmRegisterButton" disabled>
        Confirm
      </button>
      <button type="button" id="cancelButton">Cancel</button>
    </form>
  </div>
`;

export const confirmRegister = (
  identity: WebAuthnIdentity,
  alias: string
): Promise<LoginResult | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init(canisterIdPrincipal, identity, alias);
};

const tryRegister = (
  identity: WebAuthnIdentity,
  alias: string,
  pow: ProofOfWork,
  challengeResult: ChallengeResult,
  func: (result: LoginResult) => void
) => {
  withLoader(async () => {
    console.log("Registering...");
    return IIConnection.register(identity, alias, pow, challengeResult);
  }).then((result) => {
    console.log(`Result: ${result.kind}`);
    if (result.kind == "loginSuccess") {
      console.log("Result kind was success");
      // Write user number to storage
      setUserNumber(result.userNumber);

      // Congratulate user
      displayUserNumber(result.userNumber).then(() => {
        func(apiResultToLoginResult(result));
      });
    } else {
      // TODO: should we only get here on specific result.kind? like badCaptcha?
      console.log("Something didn't work, retrying");
      const loadingCaptchaText = document.querySelector(
        ".loading-captcha-text"
      ) as HTMLElement;
      loadingCaptchaText.innerHTML = "Something didn't work, please retry";
      requestCaptcha();
    }
  });
};

// TODO: disable confirm button
// TODO: add message about loading captcha
const requestCaptcha = () => {
  const form = document.getElementById("confirmForm") as HTMLFormElement;
  IIConnection.createChallenge().then((captchaResp) => {
    const captchaImg = document.querySelector("#captchaImg");
    if (captchaImg) {
      console.log("got captchaImg");
      captchaImg.setAttribute(
        "src",
        `data:image/png;base64, ${captchaResp.png_base64}`
      );
      const confirmRegisterButton = form.querySelector(
        "#confirmRegisterButton"
      ) as HTMLFormElement;

      confirmRegisterButton.removeAttribute("disabled");
      confirmRegisterButton.setAttribute(
        "data-captcha-key",
        `${captchaResp.challenge_key}`
      );
      const loadingCaptchaText = document.querySelector(
        ".loading-captcha-text"
      ) as HTMLElement;
      loadingCaptchaText.innerHTML = "please copy the chars";

      confirmRegisterButton.removeAttribute('disabled');
    }
  });
};

const init = (
  canisterIdPrincipal: Principal,
  identity: WebAuthnIdentity,
  alias: string
): Promise<LoginResult | null> => {
  requestCaptcha();

  // since the index expects to regain control we unfortunately have to wrap
  // this whole logic in a promise that then resolves (giving control back to
  // the caller)
  return new Promise((resolve) => {
    // Create a PoW before registering
    const now_in_ns = BigInt(Date.now()) * BigInt(1000000);
    const pow = getProofOfWork(now_in_ns, canisterIdPrincipal);

    const confirmRegisterButton = document.querySelector(
      "#confirmRegisterButton"
    ) as HTMLFormElement;
    const captchaInput = document.querySelector(
      "#captchaInput"
    ) as HTMLFormElement;
    const cancelButton = document.querySelector(
      "#cancelButton"
    ) as HTMLButtonElement;

    cancelButton.onclick = () => {
      resolve(null);
    };

    confirmRegisterButton.onclick = (e) => {
      e.preventDefault();
      e.stopPropagation();

      const loadingCaptchaText = document.querySelector(
        ".loading-captcha-text"
      ) as HTMLElement;
      loadingCaptchaText.innerHTML = "Checking …";
      confirmRegisterButton.setAttribute('disabled', '');


      const captchaChars = captchaInput.value;
      const captchaKey = confirmRegisterButton.dataset.captchaKey;

      const challengeResult: ChallengeResult = {
        key: Number(captchaKey),
        chars: captchaChars,
      };
      tryRegister(identity, alias, pow, challengeResult, resolve);
    };
  });
};
