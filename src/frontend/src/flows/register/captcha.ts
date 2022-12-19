import { Challenge } from "../../../generated/internet_identity_types";
import { html, render } from "lit-html";
import { LoginFlowResult, cancel } from "../../utils/flowResult";
import { apiResultToLoginFlowResult } from "../../utils/flowResult";
import { withLoader } from "../../components/loader";
import {
  IdentifiableIdentity,
  ChallengeResult,
  Connection,
} from "../../utils/iiConnection";

const pageContent = html`
  <div class="l-container c-card c-card--highlight">
    <h1 class="t-title t-title--main">Prove you're not a robot</h1>
    <form class="l-stack t-centered" id="confirmForm">
      <p class="captcha-status-text">…</p>
      <img id="captchaImg" class="c-img-block l-stack" alt="captcha image" />
      <input id="captchaInput" class="c-input" />
      <p class="t-paragraph confirm-paragraph"></p>
      <div class="c-button-group">
        <button
          type="button"
          id="cancelButton"
          class="c-button c-button--secondary"
        >
          Cancel
        </button>
        <button
          type="submit"
          class="c-button"
          id="confirmRegisterButton"
          disabled
        >
          Next
        </button>
      </div>
    </form>
  </div>
`;

export const confirmRegister = (
  connection: Connection,
  captcha: Promise<Challenge>,
  identity: IdentifiableIdentity,
  alias: string
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init(connection, identity, alias, captcha);
};

const tryRegister = (
  connection: Connection,
  identity: IdentifiableIdentity,
  alias: string,
  challengeResult: ChallengeResult,
  func: (result: LoginFlowResult) => void
) => {
  withLoader(async () => {
    return connection.register(identity, alias, challengeResult);
  }).then((registerResult) => {
    if (registerResult.kind == "badChallenge") {
      const confirmParagraph = document.querySelector(
        ".confirm-paragraph"
      ) as HTMLElement;
      confirmParagraph.innerHTML =
        "The value you entered is incorrect. A new challenge is generated.";
      requestCaptcha(connection);
    } else {
      func(apiResultToLoginFlowResult(registerResult));
    }
  });
};

// Request a captcha and, when received, update the DOM elements accordingly.
const requestCaptcha = (
  connection: Connection,
  captcha?: Promise<Challenge>
): void => {
  const form = document.getElementById("confirmForm") as HTMLFormElement;
  const captchaStatusText = document.querySelector(
    ".captcha-status-text"
  ) as HTMLElement;
  captchaStatusText.innerHTML = "Creating CAPTCHA challenge…";

  const captchaInput = document.querySelector(
    "#captchaInput"
  ) as HTMLFormElement;
  captchaInput.disabled = true;

  const confirmRegisterButton = form.querySelector(
    "#confirmRegisterButton"
  ) as HTMLFormElement;
  confirmRegisterButton.disabled = true;

  captcha = captcha || makeCaptcha(connection);

  captcha.then((captchaResp) => {
    const captchaImg = document.querySelector("#captchaImg");
    if (captchaImg) {
      captchaImg.setAttribute(
        "src",
        `data:image/png;base64, ${captchaResp.png_base64}`
      );
      confirmRegisterButton.setAttribute(
        "data-captcha-key",
        `${captchaResp.challenge_key}`
      );
      captchaStatusText.innerHTML = "Type the characters you see";
      confirmRegisterButton.disabled = false;
      captchaInput.disabled = false;
      captchaInput.value = "";
    }
  });
};

// This requests a challenge from the II backend.
export const makeCaptcha = (connection: Connection): Promise<Challenge> =>
  new Promise((resolve) => {
    setTimeout(() => {
      connection.createChallenge().then((cha) => {
        resolve(cha);
      });
    });
  });

const init = (
  connection: Connection,
  identity: IdentifiableIdentity,
  alias: string,
  captcha: Promise<Challenge>
): Promise<LoginFlowResult> => {
  requestCaptcha(connection, captcha);

  // since the index expects to regain control we unfortunately have to wrap
  // this whole logic in a promise that then resolves (giving control back to
  // the caller)
  return new Promise((resolve) => {
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
      resolve(cancel);
    };

    confirmRegisterButton.onclick = (e) => {
      e.preventDefault();
      e.stopPropagation();

      const captchaStatusText = document.querySelector(
        ".captcha-status-text"
      ) as HTMLElement;
      captchaStatusText.innerHTML = "Checking CAPTCHA challenge…";
      confirmRegisterButton.disabled = true;

      const captchaChars = captchaInput.value;
      const captchaKey = confirmRegisterButton.dataset.captchaKey;

      if (captchaKey === undefined) {
        console.log("Something went wrong: no captcha key found");
        requestCaptcha(connection);
        return;
      }

      const challengeResult: ChallengeResult = {
        key: captchaKey,
        chars: captchaChars,
      };

      tryRegister(connection, identity, alias, challengeResult, resolve);
    };
  });
};
