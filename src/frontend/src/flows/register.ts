import { WebAuthnIdentity } from "@dfinity/identity";
import { Challenge } from "../../generated/internet_identity_types";
import { html, render } from "lit-html";
import { creationOptions } from "../utils/iiConnection";
import { confirmRegister, makeCaptcha } from "./confirmRegister";
import {
  apiResultToLoginFlowResult,
  LoginFlowResult,
} from "./login/flowResult";
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

export const register = async (): Promise<LoginFlowResult | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init();
};

const renderConstructing = () => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(constructingContent, container);
};

const init = (): Promise<LoginFlowResult | null> =>
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
        // Kick-start both the captcha creation and the identity
        Promise.all([
          makeCaptcha(),
          WebAuthnIdentity.fromJSON('{"publicKey":"a5010203262001215820b6709fcb207d6e65fc8e8e4ed839fb2bda5a61f5d28275dcbfbd53050a64ab422258206d02a86accc3147673bd16f06266da200bf6895e436f0f36e495c44a9a5b1148","rawId":"01dd0f531c92533840240531ac6b79edf56462179db27412c621f989b3d8c4384f5461bc866feb1ded12b3ff36253c333e27c3acba761281f6257691a3606de428cd8a5f6fec8e3b20d54ff9bbb4d26749c87593"}'),
          //WebAuthnIdentity.create({
            //publicKey: creationOptions(),
          //}),
        ])
          .catch((error) => {
            resolve(apiResultToLoginFlowResult({ kind: "authFail", error }));
            // We can never get here, but TS doesn't understand that
            return 0 as unknown as [Challenge, WebAuthnIdentity];
          })
          .then(([captcha, identity]) => {
              console.log("Got identity");
              console.log(JSON.stringify(identity.toJSON()));
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
