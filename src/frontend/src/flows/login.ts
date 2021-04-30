import { render, html } from "lit-html";
import { aboutLink } from "../components/aboutLink";
import { confirm } from "../components/confirm";
import { icLogo } from "../components/icons";
import { withLoader } from "../components/loader";
import { logoutSection, initLogout } from "../components/logout";
import { IDPActor } from "../utils/idp_actor";
import { getUserNumber } from "../utils/userNumber";
import { LoginResult, loginUnknown } from "./loginUnknown";

const pageContent = (userNumber: bigint) => html`
  <div class="container">
    ${icLogo}
    <h1>Welcome back!</h1>
    <p>Login to manage your Internet Identity.</p>
    <div class="highlightBox">${userNumber}</div>
    <button type="button" id="login" class="primary">Login</button>
    <p style="text-align: center;">Or</p>
    <button type="button" id="loginDifferent">
      Use a different user number
    </button>
    ${logoutSection("Clear user number from browser")}
  </div>
  ${aboutLink}`;

// We retry logging in until we get a succesful user number connection pair
// If we encounter an unexpected error we reload to be safe
export const login = async (): Promise<{
  userNumber: bigint;
  connection: IDPActor;
}> => {
  try {
    const x = await tryLogin();

    switch (x.tag) {
      case "ok": {
        return { userNumber: x.userNumber, connection: x.connection };
      }
      case "err": {
        await confirm({ message: x.message, detail: x.detail });
        return login();
      }
    }
  } catch (err) {
    await confirm({
      message: "An unexpected error occured during login. Please try again",
      detail: err,
    });
    window.location.reload();
    return Promise.reject(err);
  }
};

const tryLogin = async (): Promise<LoginResult> => {
  const userNumber = getUserNumber();
  if (userNumber === undefined) {
    return loginUnknown();
  } else {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(userNumber), container);
    return init(userNumber);
  }
};

const init = async (userNumber: bigint): Promise<LoginResult> => {
  return new Promise((resolve) => {
    initLogout();
    const loginButton = document.querySelector("#login") as HTMLButtonElement;
    const loginDifferentButton = document.querySelector(
      "#loginDifferent"
    ) as HTMLButtonElement;

    loginButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      const result = await withLoader(() => IDPActor.login(userNumber));
      try {
        resolve({
          tag: "ok",
          userNumber,
          connection: result,
        });
      } catch (err) {
        resolve({
          tag: "err",
          message: `Failed to login as ${userNumber}`,
          detail: err.toString(),
        });
      }
    };

    loginDifferentButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      resolve(await loginUnknown());
    };
  });
};
