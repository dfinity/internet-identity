import { WebAuthnIdentity } from "@dfinity/identity";
import { Challenge } from "../../generated/internet_identity_types";
import { html, render } from "lit-html";
import { creationOptions } from "../utils/iiConnection";
import { confirmRegister, makeCaptcha } from "./confirmRegister";
import { apiResultToLoginResult, LoginResult } from "./loginUnknown";
import { nextTick } from "process";
import { icLogo } from "../components/icons";

const pageContent = html`
  <div class="container">
    <h1>Create a new Internet Identity Anchor</h1>
    <form id="registerForm">
      <p>Please provide a name for your device.</p>
      <input id="registerAlias" placeholder="Device name" />
      <button type="submit" class="primary">Create</button>
      <button id="registerCancel" type="button">Cancel</button>
    </form>
  </div>
`;

const constructingContent = html`
  <div class="container flex center">
    <h1>Constructing new Identity Anchor</h1>
    ${icLogo}
    <p>This may take a while. Please wait and do not refresh the page.</p>
  </div>
`;

export const register = async (): Promise<LoginResult | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init();
};

const renderConstructing = () => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(constructingContent, container);
};

const init = (): Promise<LoginResult | null> =>
  new Promise((resolve, reject) => {
    const form = document.getElementById("registerForm") as HTMLFormElement;
    const registerCancel = document.getElementById(
      "registerCancel"
    ) as HTMLButtonElement;

    registerCancel.onclick = () => resolve(null);
    form.onsubmit = async (e) => {
      e.preventDefault();
      e.stopPropagation();

      const registerAlias = form.querySelector(
        "#registerAlias"
      ) as HTMLInputElement;
      const alias = registerAlias.value;
      renderConstructing();
      await tick();

      try {
        const pendingIdentity = WebAuthnIdentity.create({
          publicKey: creationOptions(),
        });
        await tick();
        // Kick-start both the captcha creation and the identity
        Promise.all([makeCaptcha(), pendingIdentity])
          .catch((error) => {
            resolve(apiResultToLoginResult({ kind: "authFail", error }));
            // We can never get here, but TS doesn't understand that
            return 0 as unknown as [Challenge, WebAuthnIdentity];
          })
          .then(([captcha, identity]) => {
            confirmRegister(Promise.resolve(captcha), identity, alias).then(
              resolve
            );
          });
      } catch (err) {
        reject(err);
      }
    };
  });

const tick = (): Promise<void> => new Promise((resolve) => nextTick(resolve));
