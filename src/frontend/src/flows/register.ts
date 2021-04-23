import { html, render } from "lit-html";
import { confirm } from "../components/confirm";
import { withLoader } from "../components/loader";
import { IDPActor } from "../utils/idp_actor";
import { setUserId } from "../utils/userId";
import { displayUserId } from "./displayUserId";
import { LoginResult } from "./loginUnknown";

const pageContent = html`
  <div class="container">
    <h1>Register your new Internet Identity</h1>
    <form id="registerForm">
      <p>What should we call this device?</p>
      <input id="registerAlias" placeholder="Device name">
      <button type="submit" class="primary">Register</button>
      <button id="registerCancel" type="button">Cancel</button>
    </form>
  </div>
  `;

export const register = async (): Promise<LoginResult | null> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init()
}

const init = (): Promise<LoginResult | null> => new Promise(resolve => {
  const form = document.getElementById("registerForm") as HTMLFormElement;
  const registerCancel = document.getElementById("registerCancel") as HTMLButtonElement;
  
  registerCancel.onclick = () => resolve(null)
  form.onsubmit = async (e) => {
    // Enter pending state
    e.preventDefault();
    e.stopPropagation();

    // Read values from inputs
    const registerAlias = form.querySelector(
      "#registerAlias"
    ) as HTMLInputElement;

    // Send values through actor
    const { userId, connection } = await withLoader(async () => IDPActor.register(registerAlias.value));
      // TODO: Display error here
    setUserId(userId);
    await displayUserId(userId)
    resolve({ tag: "ok", connection, userId })
  };
})
