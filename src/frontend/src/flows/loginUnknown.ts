import { render, html } from "lit-html";
import { IDPActor, ApiResult } from "../utils/idp_actor";
import { parseUserNumber, setUserNumber } from "../utils/userNumber";
import { withLoader } from "../components/loader";
import { register } from "./register";
import { icLogo } from "../components/icons";
import { addDeviceUserNumber } from "./addDeviceUserNumber";
import { aboutLink } from "../components/aboutLink";
import { bannerFromIntent, UserIntent } from "../utils/userIntent";
import { nextTick } from "process";

const pageContent = (userIntent: string) => html` <style>
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
    <p>Provide your user number to login and ${userIntent}.</p>
    <form id="loginForm">
      <input
        type="text"
        id="registerUserNumber"
        placeholder="Enter User Number"
      />
      <button type="submit" id="loginButton" class="primary">Login</button>
    </form>
    <div class="textLink" id="registerSection">
      New user?
      <button id="registerButton" class="linkStyle">
        Register with Internet Identity.
      </button>
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
      title: string;
      message: string;
      detail?: string;
    };

export const loginUnknown = async (
  userIntent: UserIntent
): Promise<LoginResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(bannerFromIntent(userIntent)), container);
  // Focus the User Number Input after render
  await nextTick(() => {
    (document.querySelector("#registerUserNumber") as
      | HTMLInputElement
      | undefined)?.focus();
  });

  return new Promise((resolve, reject) => {
    initLogin(resolve);
    initLinkDevice();
    initRegister(resolve, reject);
  });
};

const initRegister = (resolve, reject) => {
  const loginForm = document.getElementById("loginForm") as HTMLFormElement;
  loginForm.onsubmit = (e) => {
    e.preventDefault();
    register()
      .then((res) => {
        if (res === null) {
          window.location.reload();
        } else {
          resolve(res);
        }
      })
      .catch(reject);
    return false;
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
        title: "Unknown user",
        message: `Failed to find an identity for the user number ${result.userNumber}. Please check your user number and try again.`,
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
  }
};
