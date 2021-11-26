import { render, html } from "lit-html";
import { IIConnection, ApiResult } from "../utils/iiConnection";
import { parseUserNumber, setUserNumber } from "../utils/userNumber";
import { withLoader } from "../components/loader";
import { register } from "./register";
import { icLogo } from "../components/icons";
import { addDeviceUserNumber } from "./addDeviceUserNumber";
import { navbar } from "../components/navbar";
import { UserIntent, authenticateUnknownIntent } from "../utils/userIntent";
import { useRecovery } from "./recovery/useRecovery";

const pageContent = (userIntent: UserIntent) => html` <style>
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
  </style>
  <div class="container">
    ${icLogo}
    <h2 id="loginWelcome">Welcome to<br />Internet Identity</h2>
    <p>
      Provide an Identity Anchor to
      authenticate${authenticateUnknownIntent(userIntent)}.
    </p>
    <input
      type="text"
      id="registerUserNumber"
      placeholder="Enter Identity Anchor"
    />
    <button type="button" id="loginButton" class="primary">Authenticate</button>
    ${userIntent.kind === "addDevice"
      ? html`<div class="spacer"></div>`
      : html`<div class="textLink" id="registerSection">
            New?
            <button id="registerButton" class="linkStyle">
              Create an Internet Identity Anchor.
            </button>
          </div>
          <div class="textLink">
            Already have an anchor
            <button id="addNewDeviceButton" class="linkStyle">
              but using a new device?
            </button>
          </div>
          <div class="textLink">
            Lost access
            <button id="recoverButton" class="linkStyle">
              and want to recover?
            </button>
          </div>`}
  </div>
  ${navbar}`;

export type LoginResult =
  | {
      tag: "ok";
      userNumber: bigint;
      connection: IIConnection;
    }
  | {
      tag: "err";
      title: string;
      message: string;
      detail?: string;
    };

export const loginUnknown = async (
  userIntent: UserIntent
): Promise<LoginResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userIntent), container);
  return new Promise((resolve, reject) => {
    initLogin(resolve);
    if (userIntent.kind !== "addDevice") {
      initLinkDevice();
      initRegister(resolve, reject);
      initRecovery();
    }
  });
};

const initRegister = (
  resolve: (res: LoginResult) => void,
  reject: (err: Error) => void
) => {
  const registerButton = document.getElementById(
    "registerButton"
  ) as HTMLButtonElement;
  registerButton.onclick = () => {
    register()
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

const initRecovery = () => {
  const recoverButton = document.getElementById(
    "recoverButton"
  ) as HTMLButtonElement;
  recoverButton.onclick = () => useRecovery();
};

const initLogin = (resolve: (res: LoginResult) => void) => {
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
    const result = await withLoader(() => IIConnection.login(userNumber));
    if (result.kind === "loginSuccess") {
      setUserNumber(userNumber);
    }
    resolve(apiResultToLoginResult(result));
  };
};

const initLinkDevice = () => {
  const addNewDeviceButton = document.getElementById(
    "addNewDeviceButton"
  ) as HTMLButtonElement;

  addNewDeviceButton.onclick = () => {
    const userNumberInput = document.getElementById(
      "registerUserNumber"
    ) as HTMLInputElement;

    const userNumber = parseUserNumber(userNumberInput.value);
    addDeviceUserNumber(userNumber);
  };
};

export const apiResultToLoginResult = (result: ApiResult): LoginResult => {
  switch (result.kind) {
    case "loginSuccess": {
      return {
        tag: "ok",
        userNumber: result.userNumber,
        connection: result.connection,
      };
    }
    case "authFail": {
      return {
        tag: "err",
        title: "Failed to authenticate",
        message:
          "We failed to authenticate you using your security device. If this is the first time you're trying to log in with this device, you have to add it as a new device first.",
        detail: result.error.message,
      };
    }
    case "unknownUser": {
      return {
        tag: "err",
        title: "Unknown Identity Anchor",
        message: `Failed to find an identity for the Identity Anchor ${result.userNumber}. Please check your Identity Anchor and try again.`,
        detail: "",
      };
    }
    case "apiError": {
      return {
        tag: "err",
        title: "We couldn't reach Internet Identity",
        message:
          "We failed to call the Internet Identity service, please try again.",
        detail: result.error.message,
      };
    }
    case "registerNoSpace": {
      return {
        tag: "err",
        title: "Failed to register",
        message:
          "Failed to register with Internet Identity, because there is no space left at the moment. We're working on increasing the capacity.",
      };
    }
    case "badChallenge": {
      return {
        tag: "err",
        title: "Failed to register",
        message:
          "Failed to register with Internet Identity, because the challenge wasn't successful",
      };
    }
    case "seedPhraseFail": {
      return {
        tag: "err",
        title: "Invalid Seed Phrase",
        message:
          "Failed to recover using this seedphrase. Did you enter it correctly?",
      };
    }
  }
};
