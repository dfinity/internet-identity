import { render, html } from "lit-html";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { icLogo } from "../../components/icons";
import { withLoader } from "../../components/loader";
import { logoutSection, initLogout } from "../../components/logout";
import { Connection } from "../../utils/iiConnection";
import { loginUnknownAnchor } from "./unknownAnchor";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { useRecovery } from "../recovery/useRecovery";
import { startCardAnimation } from "../../utils/animation";

const pageContent = (userNumber: bigint) => html`
  <div class="l-container c-card c-card--bg">
    <div class="c-card-bg">
      <canvas class="c-card-bg__canvas" width="32" height="32"></canvas>
    </div>
    <div class="c-logo">${icLogo}</div>
    <hgroup class="l-section">
      <h1 class="t-title t-title--main">Welcome back!</h1>
      <p class="t-lead">Authenticate using Internet Identity.</p>
    <hgroup>
    <div class="l-section">
      <div class="c-animated-input">
        <output class="c-animated-input__input c-input c-input--readonly t-vip" aria-label="User Number" data-usernumber>${userNumber}</output>
        <button type="button" id="login" class="c-animated-input__button c-button">Authenticate</button>
        <canvas class="c-animated-input__bg" width="32" height="32"></canvas>
      </div>
      <div class="l-divider l-divider--text" aria-label="Other Options">
        <span class="l-divider__label" aria-hidden>Or</span>
      </div>
    </div>
    <button type="button" id="loginDifferent" class="c-button c-button--secondary">
      Use a different Identity Anchor
    </button>
    <div class="l-section">
      <p>
        Lost access
        <a id="recoverButton" class="t-link">and want to recover?</a>
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
  startCardAnimation();
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
