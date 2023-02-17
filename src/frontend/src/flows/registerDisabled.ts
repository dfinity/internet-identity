import { html, render } from "lit-html";
import { warnBox } from "../components/warnBox";
import { LoginFlowCanceled, cancel } from "../utils/flowResult";
import { mainWindow } from "../components/mainWindow";
import { LEGACY_II_URL } from "../config";

const pageContent = (onCancel: () => void) => {
  const pageContentSlot = html` <hgroup>
      <h1 class="t-title t-title--main">Registration Disabled</h1>
    </hgroup>
    <div class="l-stack">
      ${warnBox({
        title: "Registration Disabled",
        message: html`<p class="t-paragraph t-lead">
            To keep you safe, we disabled registration from this address. If you
            want to securely create a new Internet Identity, visit:
            <a class="t-link" href=${LEGACY_II_URL}>${LEGACY_II_URL}</a>.
          </p>
          <p class="t-paragraph">
            If you were redirected here by another website, please inform the
            developers. Read more about this security measure
            <a
              class="t-link"
              href="https://forum.dfinity.org/t/internet-identity-proposal-to-deprecate-account-creation-on-all-origins-other-than-https-identity-ic0-app/9760"
              >here</a
            >.
          </p>`,
      })}
    </div>
    <div class="l-stack">
      <button class="c-button" @click="${onCancel}" id="deviceAliasCancel">
        Cancel
      </button>
    </div>`;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: pageContentSlot,
  });
};

export const registerDisabled = (): Promise<LoginFlowCanceled> => {
  return new Promise((resolve) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(
      pageContent(() => {
        resolve(cancel);
      }),
      container
    );
  });
};
