import { html, render } from "lit-html";

const pageContent = (userNumber) => html`
  <div class="container">
    <h1>Congratulations!</h1>
    <p>You're now registered. Keep your User Number safe. It is your responsibility.</p>
    <p>Memorize this number. Save a backup in multiple places. Without it, you can not recover your internet identity.</p>
    <label>User Number:</label>
    <div class="highlightBox">
      ${userNumber}
    </div>
    <button id="displayUserContinue" class="primary">Continue</button>
  </div>
  `;

export const displayUserNumber = async (userNumber: bigint): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init()
}

const init = (): Promise<void> => new Promise(resolve => {
  const displayUserContinue = document.getElementById("displayUserContinue") as HTMLButtonElement;
  displayUserContinue.onclick = () => resolve()
});
