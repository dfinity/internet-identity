import { render, html } from "lit-html";
import { aboutLink } from "../components/aboutLink";
import { displayError } from "../components/displayError";
import { icLogo } from "../components/icons";
import { withLoader } from "../components/loader";
import { logoutSection, initLogout } from "../components/logout";
import { IIConnection } from "../utils/iiConnection";
import { bannerFromIntent, UserIntent } from "../utils/userIntent";
import { getUserNumber } from "../utils/userNumber";
import {
  apiResultToLoginResult,
  LoginResult,
  loginUnknown,
} from "./loginUnknown";

const pageContent = (userNumber: bigint, userIntent: UserIntent) => html` <div
    class="container"
  >
    ${icLogo}
    <h1>Welcome back!</h1>
    <p>Login to ${bannerFromIntent(userIntent)}.</p>
    <div class="highlightBox">${userNumber}</div>
    <button type="button" id="login" class="primary">Login</button>
    <p style="text-align: center;">Or</p>
    <button type="button" id="loginDifferent">
      Use a different user number
    </button>
    ${logoutSection("Clear user number from browser")}
  </div>
  ${aboutLink}`;

// We retry logging in until we get a successful user number connection pair
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
      message: "An unexpected error occured during login. Please try again",
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
  });
};
