import { html, render } from "lit-html";

const pageContent = (userNumber) => html`
  <div class="container">
    <h1>Congratulations!</h1>
    <p>You are now registered. Please remember and write down your user number.</p>
    <p>You will need it to use your Internet Identity or register additional devices.</p>
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
