import { WebAuthnIdentity } from "@dfinity/identity";
import { html, render } from "lit-html";
import { withLoader } from "../components/loader";
import { IDPActor } from "../utils/idp_actor";
import { setUserNumber } from "../utils/userNumber";
import { confirmRegister } from "./confirmRegister";
import { displayUserNumber } from "./displayUserNumber";
import { LoginResult } from "./loginUnknown";

const pageContent = html`
  <div class="container">
    <h1>Register your new Internet Identity</h1>
    <form id="registerForm">
      <p>What should we call this device?</p>
      <input id="registerAlias" placeholder="Device name" />
      <button type="submit" class="primary">Register</button>
      <button id="registerCancel" type="button">Cancel</button>
    </form>
  </div>
`;

export const register = async (): Promise<LoginResult | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init();
};

const init = (): Promise<LoginResult | null> =>
  new Promise((resolve) => {
    const form = document.getElementById("registerForm") as HTMLFormElement;
    const registerCancel = document.getElementById(
      "registerCancel"
    ) as HTMLButtonElement;

    registerCancel.onclick = () => resolve(null);
    form.onsubmit = async (e) => {
      // Enter pending state
      e.preventDefault();
      e.stopPropagation();

      // Read values from inputs
      const registerAlias = form.querySelector(
        "#registerAlias"
      ) as HTMLInputElement;

      const identity = await WebAuthnIdentity.create();
      if (await confirmRegister()) {
        // Send values through actor
        const { userNumber, connection } = await withLoader(async () =>
          IDPActor.register(identity, registerAlias.value)
        );
        setUserNumber(userNumber);
        await displayUserNumber(userNumber);
        resolve({ tag: "ok", connection, userNumber });
      } else {
        resolve(null);
      }
    };
  });
