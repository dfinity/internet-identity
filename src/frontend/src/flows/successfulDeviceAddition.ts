import { html, render } from "lit-html";

const pageContent = (alias) => html`
  <div class="container">
    <h1>Success!</h1>
    <p>You’ve successfully added a new device. You can close this window and return to your other device.</p>
    <label>Device name:</label>
    <div class="userIdBox">
      ${alias}
    </div>
  </div>
  `;

export const successfullyAddedDevice = async (alias: string): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(alias), container);
}
