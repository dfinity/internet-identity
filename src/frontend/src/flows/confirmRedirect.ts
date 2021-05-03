import { html, render } from "lit-html";
import { FrontendHostname } from "../../generated/idp_types";

const pageContent = (hostName) => html`
  <style>
    #confirmRedirectHostname {
      font-size: 0.875rem;
      font-weight: 400;
    }
  </style>
  <div class="container">
    <h1>Authorize Login</h1>
    <p>Proceed to login in to:</p>
    <div id="confirmRedirectHostname" class="highlightBox">${hostName}</div>
    <button id="confirmRedirect" class="primary">Proceed</button>
    <button id="cancelRedirect">Cancel</button>
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
