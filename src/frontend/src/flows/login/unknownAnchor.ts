import { render, html } from "lit-html";
import { Connection, IIConnection } from "../../utils/iiConnection";
import { parseUserNumber, setUserNumber } from "../../utils/userNumber";
import { withLoader } from "../../components/loader";
import { icLogo } from "../../components/icons";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { useRecovery } from "../recovery/useRecovery";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { addRemoteDevice } from "../addDevice/welcomeView";
import { registerIfAllowed } from "../../utils/registerAllowedCheck";

const pageContent = () => html` <style>
    #registerUserNumber:focus {
      box-sizing: border-box;
      border-style: double;
      border-width: 2px;
      border-radius: 4px;
      border-image-slice: 1;
      outline: none;
      border-image-source: linear-gradient(
        270.05deg,
        #29abe2 10.78%,
        #522785 22.2%,
        #ed1e79 42.46%,
        #f15a24 59.41%,
        #fbb03b 77.09%
      );
    }

    #registerSection {
      margin-top: 4rem;
    }

    .spacer {
      height: 2rem;
    }

    .list-reset li {
      list-style: none;
    }
    .input {
      box-sizing: border-box;
    }
    .input--fullwidth {
      width: 100%;
    }
  </style>
  <section class="container" aria-label="Authentication">
    ${icLogo}

    <article>
      <h1 id="loginWelcome">Welcome to<br />Internet Identity</h1>
      <p>Provide an Identity Anchor to authenticate.</p>
      <input
        type="text"
        class="input input--fullwidth"
        id="registerUserNumber"
        placeholder="Enter Identity Anchor"
      />
      <button type="button" id="loginButton" class="primary">
        Authenticate
      </button>
    </article>

    <aside aria-label="Other actions">
      <ul class="list-reset">
        <li class="textLink" id="registerSection">
          New?
          <a id="registerButton" class="linkStyle">
            Create an Internet Identity Anchor.
          </a>
        </li>
        <li class="textLink">
          Already have an anchor
          <a id="addNewDeviceButton" class="linkStyle">
            but using a new device?
          </a>
        </li>
        <li class="textLink">
          Lost access
          <a id="recoverButton" class="linkStyle"> and want to recover? </a>
        </li>
      </ul>
    </aside>
    ${navbar}
  </section>
  ${footer}`;

export const loginUnknownAnchor = async (
  conn: Connection
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return new Promise((resolve, reject) => {
    initLogin(conn, resolve);
    initLinkDevice(conn);
    initRegister(conn, resolve, reject);
    initRecovery(conn);
  });
};

const initRegister = (
  conn: Connection,
  resolve: (res: LoginFlowResult) => void,
  reject: (err: Error) => void
) => {
  const registerButton = document.getElementById(
    "registerButton"
  ) as HTMLButtonElement;
  registerButton.onclick = () => {
    registerIfAllowed(conn)
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

const initRecovery = (conn: Connection) => {
  const recoverButton = document.getElementById(
    "recoverButton"
  ) as HTMLAnchorElement;
  recoverButton.onclick = () => useRecovery(conn);
};

const initLogin = (
  conn: Connection,
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

  loginButton.onclick = async () => {
    const userNumber = parseUserNumber(userNumberInput.value);
    if (userNumber === null) {
      return resolve({
        tag: "err",
        title: "Please enter a valid Identity Anchor",
        message: `${userNumber} doesn't parse as a number`,
      });
    }
    const result = await withLoader(() => conn.login(userNumber));
    if (result.kind === "loginSuccess") {
      setUserNumber(userNumber);
    }
    resolve(apiResultToLoginFlowResult(result));
  };
};

const initLinkDevice = (conn: Connection) => {
  const addNewDeviceButton = document.getElementById(
    "addNewDeviceButton"
  ) as HTMLButtonElement;

  addNewDeviceButton.onclick = async () => {
    const userNumberInput = document.getElementById(
      "registerUserNumber"
    ) as HTMLInputElement;

    const userNumber = parseUserNumber(userNumberInput.value);
    await addRemoteDevice(conn, userNumber);
  };
};
