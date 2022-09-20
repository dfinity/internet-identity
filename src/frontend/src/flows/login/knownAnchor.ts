import { render, html } from "lit";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { icLogo } from "../../components/icons";
import { withLoader } from "../../components/loader";
import { logoutSection, initLogout } from "../../components/logout";
import { Connection } from "../../utils/iiConnection";
import { loginUnknownAnchor } from "./unknownAnchor";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { useRecovery } from "../recovery/useRecovery";

const pageContent = (userNumber: bigint) => html`
  <div class="l-container c-card c-card--highlight">
    <div class="c-logo">${icLogo}</div>
    <hgroup class="l-stack">
      <h1 class="t-title t-title--main">Welcome back!</h1>
      <!--  paragraph is hidden but included for accessibility -->
      <p class="t-lead is-hidden">Authenticate using Internet Identity.</p>
    <hgroup>
    <div class="l-stack">
      <output class="c-input c-input--vip c-input--readonly t-vip" aria-label="User Number" data-usernumber>${userNumber}</output>
      <button type="button" id="login" class="c-button">Authenticate</button>
      <div class="l-divider l-divider--text" aria-label="Other Options">
        <span class="l-divider__label" aria-hidden>Or</span>
      </div>
    </div>
    <button type="button" id="loginDifferent" class="c-button c-button--secondary">
      Use a different Identity Anchor
    </button>
    <div class="l-stack">
      <p>
        <a id="recoverButton" class="t-link">Lost Access?</a>
      </p>
    </div>
    ${logoutSection("Clear Identity Anchor from browser")}
    ${navbar}
  </div>
  ${footer}`;

export const loginKnownAnchor = async (
  userNumber: bigint,
  connection: Connection
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init(userNumber, connection);
};

const init = async (
  userNumber: bigint,
  connection: Connection
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
      const result = await withLoader(() => connection.login(userNumber));
      resolve(apiResultToLoginFlowResult(result));
    };

    loginDifferentButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      resolve(await loginUnknownAnchor(connection));
    };
    const recoverButton = document.getElementById(
      "recoverButton"
    ) as HTMLAnchorElement;
    recoverButton.onclick = () => useRecovery(connection, userNumber);
  });
};
