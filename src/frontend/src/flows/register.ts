import { WebAuthnIdentity } from "@dfinity/identity";
import { Challenge } from "../../generated/internet_identity_types";
import { html, render } from "lit-html";
import {
  IdentifiableIdentity,
  DummyIdentity,
  creationOptions,
  Connection,
} from "../utils/iiConnection";
import { confirmRegister, makeCaptcha } from "./confirmRegister";
import {
  apiResultToLoginFlowResult,
  LoginFlowResult,
  cancel,
} from "./login/flowResult";
import { nextTick } from "process";
import { icLogo } from "../components/icons";
import { validateAlias } from "./addDevice/validateAlias";

const pageContent = html`
  <div class="l-container c-card c-card--highlight">
    <hgroup>
      <div class="c-logo">${icLogo}</div>
      <p class="t-lead t-centered t-paragraph l-stack">
        What device are you using?
      </p>
    </hgroup>
    <form id="registerForm" class="l-stack">
      <input
        id="registerAlias"
        placeholder="Device name"
        aria-label="device name"
        type="text"
        required
        maxlength="30"
        pattern="^[A-Za-z0-9]+((-|\\s|_)*[A-Za-z0-9])*$"
        spellcheck="false"
        class="c-input"
      />
      <div class="c-button-group">
        <button
          id="registerCancel"
          type="button"
          class="c-button c-button--secondary"
        >
          Cancel
        </button>
        <button type="submit" class="c-button">Next</button>
      </div>
    </form>
  </div>
`;

const constructingContent = html`
  <div class="l-container c-card c-card--highlight">
    <div class="c-logo">${icLogo}</div>
    <p class="t-lead t-centered t-paragraph l-stack">
      Creating your Identity Anchor. Do not refresh the page.
    </p>
  </div>
`;

export const register = async (
  connection: Connection
): Promise<LoginFlowResult> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init(connection);
};

export const renderConstructing = (): void => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(constructingContent, container);
};

const init = (connection: Connection): Promise<LoginFlowResult> =>
  new Promise((resolve, reject) => {
    const form = document.getElementById("registerForm") as HTMLFormElement;
    const registerCancel = document.getElementById(
      "registerCancel"
    ) as HTMLButtonElement;

    registerCancel.onclick = () => resolve(cancel);
    form.onsubmit = async (e) => {
      e.preventDefault();
      e.stopPropagation();

      const registerAlias = form.querySelector(
        "#registerAlias"
      ) as HTMLInputElement;
      const alias = registerAlias.value;
      renderConstructing();
      await tick();

      /* The Identity (i.e. key pair) used when creating the anchor.
       * If "II_DUMMY_AUTH" is set, we create a dummy identity. The same identity must then be used in iiConnection when authenticating.
       */
      const createIdentity =
        process.env.II_DUMMY_AUTH === "1"
          ? () => Promise.resolve(new DummyIdentity())
          : () =>
              WebAuthnIdentity.create({
                publicKey: creationOptions(),
              });

      try {
        // Kick-start both the captcha creation and the identity
        Promise.all([
          makeCaptcha(connection),
          createIdentity() as Promise<IdentifiableIdentity>,
        ])
          .catch((error) => {
            resolve(apiResultToLoginFlowResult({ kind: "authFail", error }));
            // We can never get here, but TS doesn't understand that
            return 0 as unknown as [Challenge, IdentifiableIdentity];
          })
          .then(([captcha, identity]) => {
            confirmRegister(
              connection,
              Promise.resolve(captcha),
              identity,
              alias
            ).then(resolve);
          });
      } catch (err) {
        reject(err);
      }
    };

    const registerAlias = document.getElementById(
      "registerAlias"
    ) as HTMLInputElement;

    registerAlias.addEventListener("invalid", () => {
      const message = validateAlias(
        registerAlias.validity,
        registerAlias.value
      );
      registerAlias.setCustomValidity(message);
    });

    registerAlias.addEventListener("input", () => {
      registerAlias.setCustomValidity("");
      registerAlias.reportValidity();
    });
  });

const tick = (): Promise<void> => new Promise((resolve) => nextTick(resolve));
