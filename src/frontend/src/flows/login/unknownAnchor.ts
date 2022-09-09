import { render, html } from "lit-html";
import { Connection } from "../../utils/iiConnection";
import { parseUserNumber, setUserNumber } from "../../utils/userNumber";
import { withLoader } from "../../components/loader";
import { forwardIcon, icLogo } from "../../components/icons";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { useRecovery } from "../recovery/useRecovery";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { addRemoteDevice } from "../addDevice/welcomeView";
import { registerIfAllowed } from "../../utils/registerAllowedCheck";
import { footerLinksContent } from "../../components/footerLinks";
import {
  aboutButtonContent,
  aboutModalContent,
  initAboutModal,
} from "../../components/aboutModal";
import { beep } from "../../utils/sound";

const pageContent = () => html`
  <section class="l-container c-card c-card--highlight" aria-label="Authentication">
    <div class="c-logo">${icLogo}</div>
    <article class="">
      <hgroup>
        <h1 id="loginWelcome" class="t-title t-title--main">Internet Identity</h1>
      <hgroup>
      
      <div class="l-section">
        <div class="c-connect-input">
          <input
            type="text"
            class="c-input c-input--vip c-connect-input__input"
            id="registerUserNumber"
            placeholder="Enter Anchor"
          />
          <button type="button" id="loginButton" class="c-button c-connect-input__button">
            ${forwardIcon}
          </button>
      </div>
    </article>

    ${aboutButtonContent} ${aboutModalContent} ${footerLinksContent}

    <!-- ${navbar} -->
  </section>
  ${footer}`;

export const loginUnknownAnchor = async (
  connection: Connection
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return new Promise((resolve, reject) => {
    initLogin(connection, resolve);
    initLinkDevice(connection);
    initRegister(connection, resolve, reject);
    initRecovery(connection);
    initAboutModal();
  });
};

const initRegister = (
  connection: Connection,
  resolve: (res: LoginFlowResult) => void,
  reject: (err: Error) => void
) => {
  const registerButton = document.getElementById(
    "registerButton"
  ) as HTMLButtonElement;
  registerButton.onclick = () => {
    registerIfAllowed(connection)
      .then((res) => {
        if (res === null) {
          window.location.reload();
        } else {
          resolve(res);
        }
      })
      .catch(reject);
  };
};

const initRecovery = (connection: Connection) => {
  const recoverButton = document.getElementById(
    "recoverButton"
  ) as HTMLAnchorElement;
  recoverButton.onclick = () => useRecovery(connection);
};

const initLogin = (
  connection: Connection,
  resolve: (res: LoginFlowResult) => void
) => {
  const userNumberInput = document.getElementById(
    "registerUserNumber"
  ) as HTMLInputElement;
  const loginButton = document.getElementById(
    "loginButton"
  ) as HTMLButtonElement;

  userNumberInput.onkeypress = (e) => {
    // submit if user hits enter
    if (e.key === "Enter") {
      e.preventDefault();
      loginButton.click();
    }

    const code = e.which ?? e.keyCode;
    if (code > 31 && (code < 48 || code > 57)) {
      beep(100, 700, 15);
      e.preventDefault();
    }
  };

  // always select the input
  userNumberInput.select();

  loginButton.onclick = async () => {
    // not empty validation
    if (userNumberInput.value.length === 0) {
      beep(200, 150, 35);

      let f = 1;
      const i = setInterval(function () {
        if (f >= 6)
          // even only
          clearInterval(i);
        f++;
        document
          .getElementById("registerButton")
          ?.classList.toggle("very-faded");
      }, 150);

      return;
    }

    const userNumber = parseUserNumber(userNumberInput.value);
    if (userNumber === null) {
      return resolve({
        tag: "err",
        title: "Please enter a valid Identity Anchor",
        message: `${userNumber} doesn't parse as a number`,
      });
    }
    const result = await withLoader(() => connection.login(userNumber));
    if (result.kind === "loginSuccess") {
      setUserNumber(userNumber);
    }
    resolve(apiResultToLoginFlowResult(result));
  };
};

const initLinkDevice = (connection: Connection) => {
  const addNewDeviceButton = document.getElementById(
    "addNewDeviceButton"
  ) as HTMLButtonElement;

  addNewDeviceButton.onclick = async () => {
    const userNumberInput = document.getElementById(
      "registerUserNumber"
    ) as HTMLInputElement;

    const userNumber = parseUserNumber(userNumberInput.value);
    await addRemoteDevice(userNumber, connection);
  };
};
