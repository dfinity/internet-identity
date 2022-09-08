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

const pageContent = (userNumber: bigint) => html`
  <div class="l-container c-card c-card--highlight">
    <div class="c-logo">${icLogo}</div>
    <hgroup class="l-section">
      <h1 class="t-title t-title--main">Welcome back!</h1>
      <p class="t-lead">Authenticate using Internet Identity.</p>
    <hgroup>
    <div class="l-section">
      <output class="c-input c-input--readonly t-vip" aria-label="User Number" data-usernumber>${userNumber}</output>
      <button type="button" id="login" class="c-button">Authenticate</button>      
    </div>


    <a href="/about" class="c-button info-button">ℹ</a>

    <aside aria-label="Other actions">
      <ul class="t-discreet c-list c-list--inline">
        <li class="textLink" id="registerSection">
          <a id="registerButton" class="t-link">Create New Anchor</a>
        </li>
        <li class="textLink">
          <a id="addNewDeviceButton" class="t-link">Associate Device</a>
        </li>
        <li class="textLink">
          <a id="browserCompatibilityButton" class="t-link"
            >Browser Compatibility</a
          >
        </li>
        <li class="textLink">
          <a id="faqLink" class="t-link" href="/faq">FAQ</a>
        </li>
        <li class="textLink">
          <a id="recoverButton" class="t-link">Lost Access?</a>
        </li>
      </ul>
    </aside>

    <!--${navbar}-->
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
