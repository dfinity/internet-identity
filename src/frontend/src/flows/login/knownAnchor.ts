import { render, html } from "lit-html";
import { navbar } from "../../components/navbar";
import { footer } from "../../components/footer";
import { icLogo } from "../../components/icons";
import { withLoader } from "../../components/loader";
import { initLogout, logoutSection } from "../../components/logout";
import { Connection } from "../../utils/iiConnection";
import { loginUnknownAnchor } from "./unknownAnchor";
import { apiResultToLoginFlowResult, LoginFlowResult } from "./flowResult";
import { useRecovery } from "../recovery/useRecovery";
import { footerLinksContent } from "../../components/footerLinks";
import {
  aboutButtonContent,
  aboutModalContent,
  initAboutModal,
} from "../../components/aboutModal";

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

    <button type="button" id="loginDifferent" class="c-button c-button--secondary">
      Use a different Identity Anchor
    </button>
    
    ${aboutButtonContent} ${aboutModalContent} ${footerLinksContent}
    ${logoutSection("Clear Anchor from browser")}

    <!--${navbar}-->
  </div>
  ${footer}`;

export const loginKnownAnchor = async (
  userNumber: bigint,
  connection: Connection
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);

  initAboutModal();

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

      const title = document.querySelector(".t-title--main") as HTMLElement;
      const titleValue = title.innerText;
      title.innerText = "Connecting...";

      const result = await withLoader(
        () => connection.login(userNumber),
        false
      );

      title.innerText = titleValue;

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
