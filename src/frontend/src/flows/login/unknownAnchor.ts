import { render, html } from "lit-html";
import { Connection } from "../../utils/iiConnection";
import { parseUserNumber, setUserNumber } from "../../utils/userNumber";
import { withLoader } from "../../components/loader";
import { icLogo } from "../../components/icons";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { useRecovery } from "../recovery/useRecovery";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { addRemoteDevice } from "../addDevice/welcomeView";
import { registerIfAllowed } from "../../utils/registerAllowedCheck";

const pageContent = () => html`
  <section class="l-container c-card c-card--highlight" aria-label="Authentication">
    <div class="c-logo">${icLogo}</div>
    <article class="l-section">
      <hgroup>
        <h1 id="loginWelcome" class="t-title t-title--main">Welcome to<br />Internet Identity</h1>
        <p class="t-lead">Provide an Identity Anchor to authenticate.</p>
      <hgroup>
      <input
        type="text"
        class="c-input c-input--vip"
        id="registerUserNumber"
        placeholder="Enter Anchor"
      />
      <button type="button" id="loginButton" class="c-button">
        Authenticate
      </button>
    </article>

    <aside class="l-section l-section--spacious" aria-label="Other actions">
      <ul class="t-discreet c-list">
        <li class="textLink" id="registerSection">
          New?
          <a id="registerButton" class="t-link">Create an Internet Identity Anchor.</a>
        </li>
        <li class="textLink">
          Already have an anchor
          <a id="addNewDeviceButton" class="t-link">
            but using a new device?
          </a>
        </li>
        <li class="textLink">
          Lost access
          <a id="recoverButton" class="t-link">
            and want to recover?
          </a>
        </li>
      </ul>
    </aside>
    ${navbar}
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
  };

  // always select the input
  userNumberInput.select();

  loginButton.onclick = async () => {
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
