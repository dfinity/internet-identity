import { html, render } from "lit-html";
import { warnBox } from "../components/warnBox";
import { LoginFlowCanceled, cancel } from "../utils/flowResult";
import { mainWindow } from "../components/mainWindow";

const pageContent = (onCancel: () => void) => mainWindow({
  showFooter: false,
  showLogo: false,
  slot: html`
  <hgroup>
    <h1 class="t-title t-title--main">
      Create a new Internet Identity Anchor
    </h1>
  </hgroup>
  <div class="l-stack">
    ${warnBox({
      title: "Registration Disabled",
      message: html`<p class="t-paragraph t-lead">
          You are <b class="t-strong">not</b> browsing this website on the
          expected URL:
          <a class="t-link" href="https://identity.ic0.app"
            >https://identity.ic0.app</a
          >. For security reasons creation of new Internet Identity anchors is
          disabled on this origin.
        </p>
        <p class="t-paragraph">
          Please switch to
          <a class="t-link" href="https://identity.ic0.app"
            >https://identity.ic0.app</a
          >
          to register a new Internet Identity anchor.
        </p>
        <p class="t-paragraph">
          If you were redirected here by another website, please inform the
          developers. More information is provided
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
  </div>`
});

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
