import { html, render } from "lit-html";
import { warningIcon } from "../components/icons";

const pageContent = () => html`
  <style>
    #warningContainer {
      min-height: 15rem;
    }
    .warningIcon {
      align-self: center;
      width: 3rem;
      height: 3rem;
      margin-bottom: 1.5rem;
    }
    #warningHeading {
      text-align: center;
    }
    #warningContainer p {
      font-size: 1.2rem;
    }
    #warningContainer a {
      margin-bottom: 1rem;
    }
    .spacer {
      min-height: 48px;
    }
  </style>
  <div id="warningContainer" class="container">
    ${warningIcon}
    <h1 id="warningHeading">Warning</h1>
    <p>
      If you have only one device attached to an Identity Anchor and lose that
      device, you will no longer be able to use that Identity Anchor.
    </p>
    <p>
      Additionally, on iOS and MacOS, clearing your Safari browser data will
      <em>delete</em> your authentication keys from the respective device,
      disabling it.
    </p>
    <p>
      As a best practice we always recommend you add multiple devices to an
      Identity Anchor and at <em>least</em> one portable authenticator (USB
      key). You can see how to add more devices here:
    </p>
    <a
      target="_blank"
      href="https://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html"
      >https://sdk.dfinity.org/docs/ic-identity-guide/auth-how-to.html</a
    >
    <p>
      It is recommended to add multiple devices (among them at least one
      portable authenticator) to an Identity Anchor and keep the portable
      authenticator in a safe place. For more information about Internet
      Identity and the NNS, please check out our FAQ page:
    </p>
    <a target="_blank" href="https://dfinity.org/faq/"
      >https://dfinity.org/faq/</a
    >
    <button id="displayWarningPrimary" class="primary">
      Add a recovery mechanism to an Identity Anchor
    </button>
    <div class="spacer"></div>
  </div>
`;

export const displaySingleDeviceWarning = async (): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(), container);
  console.warn("single device warning rendered");
  return init();
};

const init = (): Promise<void> =>
  new Promise((resolve) => {
    const displayWarningPrimary = document.getElementById(
      "displayWarningPrimary"
    ) as HTMLButtonElement;
    displayWarningPrimary.onclick = () => resolve();
  });
