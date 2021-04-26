import { html, render } from "lit-html";

const pageContent = html`
  <div class="container">
    <h1>Finish registration</h1>
    <p>Now confirm your security device one more time to register.</p>
    <button type="button" class="primary" id="registerButton">Register</button>
    <button type="button" id="cancelButton">Cancel</button>
  </div>
  `;

export const confirmRegister = async (): Promise<boolean> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init()
}

const init = (): Promise<boolean> => new Promise(resolve => {
  const registerButton = document.getElementById("registerButton") as HTMLFormElement;
  const cancelButton = document.getElementById("cancelButton") as HTMLButtonElement;
  
  cancelButton.onclick = () => resolve(false);
  registerButton.onclick = () => resolve(true);
})
