import { html, render } from "lit-html";
import { FrontendHostname } from "../../generated/idp_types";

const pageContent = (hostName) => html`
  <div class="container">
    <h1>Confirm Login</h1>
    <p>Proceed to login in to:</p>
    <div class="userNumberBox">${hostName}</div>
    <button id="confirmRedirect" class="primary">Yes</button>
    <button id="cancelRedirect">No</button>
  </div>
  `;

export const confirmRedirect = async (hostName: FrontendHostname): Promise<boolean> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(hostName), container);
  return init()
}

const init = (): Promise<boolean> => new Promise(resolve => {
  const confirmRedirect = document.getElementById("confirmRedirect") as HTMLButtonElement;
  const cancelRedirect = document.getElementById("cancelRedirect") as HTMLButtonElement;
  confirmRedirect.onclick = () => resolve(true)
  cancelRedirect.onclick = () => resolve(false)
});
