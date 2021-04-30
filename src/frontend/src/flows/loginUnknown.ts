import { render, html } from "lit-html";
import { IDPActor, ApiResult } from "../utils/idp_actor";
import { parseUserNumber, setUserNumber } from "../utils/userNumber";
import { withLoader } from "../components/loader";
import { register } from "./register";
import { icLogo } from "../components/icons";
import { addDeviceUserNumber } from "./addDeviceUserNumber";
import { aboutLink } from "../components/aboutLink";

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

    .textLink {
      margin-bottom: 0.7rem;
      font-size: 0.95rem;
      white-space: nowrap;
    }
    .textLink button {
      font-size: 0.95rem;
      font-weight: 600;
      text-decoration: none;
      color: black;
    }
  </style>
  <div class="container">
    ${icLogo}
    <h2 id="loginWelcome">Welcome to<br />Internet Identity</h2>
    <p>Provide your user number to login with Internet Identity.</p>
    <input
      type="text"
      id="registerUserNumber"
      placeholder="Enter User Number"
    />
    <button type="button" id="loginButton" class="primary">Login</button>
    <div class="textLink" id="registerSection">
      New user?
      <button id="registerButton" class="linkStyle">Register with Internet Identity.</button>
    </div>
    <div class="textLink">
      Already registered
      <button id="addNewDeviceButton" class="linkStyle">
        but using a new device?
      </button>
    </div>
  </div>
  ${aboutLink}`;

export type LoginResult =
  | {
    tag: "ok";
    userNumber: bigint;
    connection: IDPActor;
  }
  | {
    tag: "err";
    message: string;
    detail: string;
  };

export const loginUnknown = async (): Promise<LoginResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  return new Promise((resolve, reject) => {
    initLogin(resolve);
    initLinkDevice();
    initRegister(resolve, reject);
  });
};

const initRegister = (resolve, reject) => {
  const registerButton = document.getElementById(
    "registerButton"
  ) as HTMLButtonElement;
  registerButton.onclick = () => {
    register()
      .then(res => {
        if (res === null) {
          window.location.reload();
        } else {
          resolve(res);
        }
      })
      .catch(reject)
  };
};

const initLogin = (resolve) => {
  const userNumberInput = document.getElementById(
    "registerUserNumber"
  ) as HTMLInputElement;
  const loginButton = document.getElementById(
    "loginButton"
  ) as HTMLButtonElement;

  loginButton.onclick = async () => {
    const userNumber = parseUserNumber(userNumberInput.value);
    if (userNumber === null) {
      return resolve({
        tag: "err",
        message: "Please enter a valid User Number",
        detail: `${userNumber} doesn't parse as a number`,
      });
    }
    const result = await withLoader(() => IDPActor.login(userNumber));
    if (result.kind === "loginSuccess") {
      setUserNumber(userNumber);
    }
    resolve(apiResultToLoginResult(result));
  }
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
    addDeviceUserNumber(userNumber)
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
    };
    case "authFail": {
      return {
        tag: "err",
        message: "Failed to authenticate",
        detail: result.error.message
      };
    };
    case "unknownUser": {
      return {
        tag: "err",
        message: `Failed to find an identity for the user number ${result.userNumber}`,
        detail: ""
      };
    };
    case "apiError": {
      return {
        tag: "err",
        message: "Failed to call the Internet Identity service",
        detail: result.error.message
      };
    };
    case "registerNoSpace": {
      return {
        tag: "err",
        message: "Failed to register with Internet Identity, because there is no space left",
        detail: ""
      }
    }
  }
}
