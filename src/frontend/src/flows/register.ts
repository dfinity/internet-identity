import { WebAuthnIdentity } from "@dfinity/identity";
import { html, render } from "lit-html";
import { withLoader } from "../components/loader";
import { IDPActor, canisterIdPrincipal } from "../utils/idp_actor";
import { setUserNumber } from "../utils/userNumber";
import { confirmRegister } from "./confirmRegister";
import { displayUserNumber } from "./displayUserNumber";
import { apiResultToLoginResult, LoginResult } from "./loginUnknown";
import getProofOfWork from "../crypto/pow";
import { nextTick } from "process";
import { icLogo } from "../components/icons";


const pageContent = html`
  <div class="container">
    <h1>Register your new Internet Identity</h1>
    <form id="registerForm">
      <p>Please provide a name for your device.</p>
      <input id="registerAlias" placeholder="Device name" />
      <button type="submit" class="primary">Register</button>
      <button id="registerCancel" type="button">Cancel</button>
    </form>
  </div>
`;

const constructingContent = html`
  <div class="container">
  <h1>Constructing your Internet Identity</h1>
  ${icLogo}
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
}

const init = (): Promise<LoginResult | null> =>
  new Promise((resolve, reject) => {
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
      const alias = registerAlias.value;
      renderConstructing();
      await tick();

      try {
        const pendingIdentity = WebAuthnIdentity.create().catch(error => {
          resolve(apiResultToLoginResult({kind: "authFail", error}));
          // We can never get here, but TS doesn't understand that
          return 0 as any
        });
        await tick();
        // Do PoW before registering.
        const now_in_ns = BigInt(Date.now()) * BigInt(1000000);
        const pow = getProofOfWork(now_in_ns, canisterIdPrincipal);
        const identity = await pendingIdentity;
        if (await confirmRegister()) {
          // Send values through actor
          const result = await withLoader(async () =>
            IDPActor.register(identity, alias, pow)
          );
          if (result.kind === "loginSuccess") {
            setUserNumber(result.userNumber);
            await displayUserNumber(result.userNumber);
          }
          resolve(apiResultToLoginResult(result));
        } else {
          resolve(null);
        }
      } catch (err) {
        reject(err)
      }
    };
  });

const tick = (): Promise<void> =>
  new Promise(resolve => nextTick(resolve));
