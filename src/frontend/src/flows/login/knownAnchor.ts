import { render, html } from "lit-html";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { icLogo } from "../../components/icons";
import { withLoader } from "../../components/loader";
import { logoutSection, initLogout } from "../../components/logout";
import { IIConnection } from "../../utils/iiConnection";
import { loginUnknownAnchor } from "./unknownAnchor";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { useRecovery } from "../recovery/useRecovery";

const pageContent = (userNumber: bigint) => html`
  <div class="l-container c-card">
    <div class="c-logo">${icLogo}</div>
    <hgroup>
      <h1 class="t-title t-title--main">Welcome back!</h1>
      <p class="t-lead">Authenticate using Internet Identity.</p>
    <hgroup>
    <data class="c-card c-card--narrow c-card--outline t-vip" aria-label="User Number">${userNumber}</data>
    <button type="button" id="login" class="button">Authenticate</button>
    
    <button type="button" id="loginDifferent" class="button button--secondary">
      Use a different Identity Anchor
    </button>
    <div class="c-section">
      <p>
        Lost access
        <button id="recoverButton" class="t-link">and want to recover?</button>
      </p>
    </div>
    ${logoutSection("Clear Identity Anchor from browser")} ${navbar}
  </div>
  ${footer}`;

export const loginKnownAnchor = async (
  userNumber: bigint
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init(userNumber);
};

const init = async (userNumber: bigint): Promise<LoginFlowResult> => {
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
      resolve(apiResultToLoginFlowResult(result));
    };

    loginDifferentButton.onclick = async (ev) => {
      ev.preventDefault();
      ev.stopPropagation();
      resolve(await loginUnknownAnchor());
    };
    const recoverButton = document.getElementById(
      "recoverButton"
    ) as HTMLButtonElement;
    recoverButton.onclick = () => useRecovery(userNumber);
  });
};
