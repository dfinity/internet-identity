import { html, render } from "lit-html";
import { ApiResult } from "../utils/iiConnection";
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
      <p>Please copy the characters you see below.</p>
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

export const confirmRegister = async (
  identity: WebAuthnIdentity,
  alias: string
): Promise<LoginResult | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return await init(canisterIdPrincipal, identity, alias);
};

// TODO: retry on bad captcha
const tryRegister = async (
  identity: WebAuthnIdentity,
  alias: string,
  pow: ProofOfWork,
  challengeResult: ChallengeResult
): Promise<ApiResult> => {
  const result = await withLoader(async () => {
    console.log("Registering...");
    return IIConnection.register(identity, alias, pow, challengeResult);
  });
  console.log(`Result: ${result.kind}`);
  if (result.kind == "loginSuccess") {
    console.log("Result kind was success");
    // Write user number to storage
    setUserNumber(result.userNumber);

    // Congratulate user
    await displayUserNumber(result.userNumber);
  }

  return result;
};

const init = async (
  canisterIdPrincipal: Principal,
  identity: WebAuthnIdentity,
  alias: string
): Promise<LoginResult | null> => {
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
    }
  });
  return new Promise((resolve) => {
    // Create a PoW before registering
    const now_in_ns = BigInt(Date.now()) * BigInt(1000000);
    const pow = getProofOfWork(now_in_ns, canisterIdPrincipal);

    const confirmRegisterButton = document.getElementById(
      "confirmRegisterButton"
    ) as HTMLFormElement;
    const captchaInput = document.querySelector(
      "#captchaInput"
    ) as HTMLFormElement;
    const cancelButton = document.getElementById(
      "cancelButton"
    ) as HTMLButtonElement;

    cancelButton.onclick = () => {
      resolve(null);
    };
    confirmRegisterButton.onclick = (e) => {
      e.preventDefault();
      e.stopPropagation();

      const captchaChars = captchaInput.value;
      const captchaKey = confirmRegisterButton.dataset.captchaKey;

      const challengeResult: ChallengeResult = {
        key: Number(captchaKey),
        chars: captchaChars,
      };
      tryRegister(identity, alias, pow, challengeResult).then((e) => {
        console.log("registration successful");
        resolve(apiResultToLoginResult(e));
      });
    };
  });
};
