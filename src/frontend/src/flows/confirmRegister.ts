import { html, render } from "lit-html";

const pageContent = html`
  <div class="container">
    <h1>Confirm new device</h1>
    <p>Please confirm to add your device.</p>
    <button type="button" class="primary" id="confirmRegisterButton">
      Confirm
    </button>
    <button type="button" id="cancelButton">Cancel</button>
  </div>
`;

export const confirmRegister = async (): Promise<boolean> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent, container);
  return init();
};

const init = (): Promise<boolean> =>
  new Promise((resolve) => {
    const confirmRegisterButton = document.getElementById(
      "confirmRegisterButton"
    ) as HTMLFormElement;
    const cancelButton = document.getElementById(
      "cancelButton"
    ) as HTMLButtonElement;

    cancelButton.onclick = () => resolve(false);
    confirmRegisterButton.onclick = () => resolve(true);
  });
