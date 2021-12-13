import {html, render} from "lit-html";
import {initLogout, logoutSection} from "../components/logout";
import {warningIcon} from "../components/icons";
import {LoginResult} from "./loginUnknown";

const pageContent = html`
  <div class="container">
    <h1>Create a new Internet Identity Anchor</h1>
    <div class="nagBox">
      <div class="nagIcon">${warningIcon}</div>
      <div class="nagContent">
        <div class="nagTitle">Registration Disabled</div>
        <div class="nagMessage">
          <p>
            You are <b>not</b> browsing on <a href="https://identity.ic0.app">https://identity.ic0.app</a>.
            For security reasons creation of new Internet Identity anchors is disabled
            on this origin.
          </p>
          Please switch to <a href="https://identity.ic0.app">https://identity.ic0.app</a> to register a new Internet Identity
          anchor.
        </div>
      </div>
    </div>
    <button id="deviceAliasCancel">Cancel</button>
    ${logoutSection()}
  </div>
`;

export const registerDisabled = async (): Promise<null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init();
};

const init = (): Promise<null> =>
  new Promise((resolve) => {
    initLogout();
    const deviceAliasCancel = document.getElementById(
      "deviceAliasCancel"
    ) as HTMLButtonElement;
    deviceAliasCancel.onclick = () => {
      resolve(null);
    };
  });
