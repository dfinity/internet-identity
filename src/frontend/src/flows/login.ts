import { render, html } from "lit-html";
import { navbar } from "../components/navbar";
import { displayError } from "../components/displayError";
import { icLogo } from "../components/icons";
import { withLoader } from "../components/loader";
import { logoutSection, initLogout } from "../components/logout";
import { IIConnection } from "../utils/iiConnection";
import { authenticateIntent, UserIntent } from "../utils/userIntent";
import { getUserNumber } from "../utils/userNumber";
import {
  apiResultToLoginResult,
  LoginResult,
  loginUnknown,
} from "./loginUnknown";
import { useRecovery } from "./recovery/useRecovery";

const pageContent = (
  userNumber: bigint,
  userIntent: UserIntent
) => html` <style>
    .spacer {
      height: 2rem;
    }
  </style>
  <div class="container">
    ${icLogo}
    <h1>Welcome back!</h1>
    <p>${authenticateIntent(userIntent)}.</p>
    <div class="highlightBox">${userNumber}</div>
    <button type="button" id="login" class="primary">Authenticate</button>
    <p style="text-align: center;">Or</p>
    <button type="button" id="loginDifferent">
      Use a different Identity Anchor
    </button>
    <div class="spacer"></div>
    <div class="textLink">
      Lost access
      <button id="recoverButton" class="linkStyle">and want to recover?</button>
    </div>
    ${logoutSection("Clear Identity Anchor from browser")}
  </div>
  ${navbar}`;

// We retry logging in until we get a successful Identity Anchor connection pair
// If we encounter an unexpected error we reload to be safe
export const login = async (
  userIntent: UserIntent
): Promise<{
  userNumber: bigint;
  connection: IIConnection;
}> => {
  try {
    const x = await tryLogin(userIntent);

    switch (x.tag) {
      case "ok": {
        return { userNumber: x.userNumber, connection: x.connection };
      }
      case "err": {
        await displayError({ ...x, primaryButton: "Try again" });
        return login(userIntent);
      }
    }
  } catch (err) {
    await displayError({
      title: "Something went wrong",
      message:
        "An unexpected error occurred during authentication. Please try again",
      detail: err,
      primaryButton: "Try again",
    });
    window.location.reload();
    return Promise.reject(err);
  }
};

const tryLogin = async (userIntent: UserIntent): Promise<LoginResult> => {
  const userNumber = getUserNumber();
  if (userNumber === undefined) {
    return loginUnknown(userIntent);
  } else {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(userNumber, userIntent), container);
    return init(userNumber, userIntent);
  }
};

const init = async (
  userNumber: bigint,
  userIntent: UserIntent
): Promise<LoginResult> => {
  return new Promise((resolve) => {
    initLogout();
    const loginButton = document.querySelector("#login") as HTMLButtonElement;
    const loginDifferentButton = document.querySelector(
      "#loginDifferent"
    ) as HTMLButtonElement;

    loginButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      const result = await withLoader(() => IIConnection.login(userNumber));
      resolve(apiResultToLoginResult(result));
    };

    loginDifferentButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      resolve(await loginUnknown(userIntent));
    };
    const recoverButton = document.getElementById(
      "recoverButton"
    ) as HTMLButtonElement;
    recoverButton.onclick = () => useRecovery(userNumber);
  });
};
