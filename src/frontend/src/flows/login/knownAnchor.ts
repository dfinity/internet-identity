import { render, html } from "lit-html";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { icLogo } from "../../components/icons";
import { withLoader } from "../../components/loader";
import { logoutSection, initLogout } from "../../components/logout";
import { IIConnection, Connection } from "../../utils/iiConnection";
import { loginUnknownAnchor } from "./unknownAnchor";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { useRecovery } from "../recovery/useRecovery";

const pageContent = (userNumber: bigint) => html` <style>
    .spacer {
      height: 2rem;
    }
  </style>
  <div class="container">
    ${icLogo}
    <h1>Welcome back!</h1>
    <p>Authenticate using Internet Identity.</p>
    <div class="highlightBox">${userNumber}</div>
    <button type="button" id="login" class="primary">Authenticate</button>
    <p style="text-align: center;">Or</p>
    <button type="button" id="loginDifferent">
      Use a different Identity Anchor
    </button>
    <div class="spacer"></div>
    <div class="textLink">
      Lost access
      <a id="recoverButton" class="linkStyle">and want to recover?</a>
    </div>
    ${logoutSection("Clear Identity Anchor from browser")} ${navbar}
  </div>
  ${footer}`;

export const loginKnownAnchor = async (
  conn: Connection,
  userNumber: bigint
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init(conn, userNumber);
};

const init = async (
  conn: Connection,
  userNumber: bigint
): Promise<LoginFlowResult> => {
  return new Promise((resolve) => {
    initLogout();
    const loginButton = document.querySelector("#login") as HTMLButtonElement;
    const loginDifferentButton = document.querySelector(
      "#loginDifferent"
    ) as HTMLButtonElement;

    loginButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      const result = await withLoader(() => conn.login(userNumber));
      resolve(apiResultToLoginFlowResult(result));
    };

    loginDifferentButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      resolve(await loginUnknownAnchor(conn));
    };
    const recoverButton = document.getElementById(
      "recoverButton"
    ) as HTMLAnchorElement;
    recoverButton.onclick = () => useRecovery(conn, userNumber);
  });
};
