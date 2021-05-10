import { html, render } from "lit-html";

const pageContent = (userNumber: bigint) => html`
  <div class="container">
    <h1>Congratulations!</h1>
    <p>
      Please record your user number. You will need it later to use your
      Internet Identity or to register additional devices.
    </p>
    <label>User Number:</label>
    <div class="highlightBox">${userNumber}</div>
    <button id="displayUserContinue" class="primary">Continue</button>
  </div>
`;

export const displayUserNumber = async (userNumber: bigint): Promise<void> => {
  const container = document.getElementById("pageContent") as HTMLElement;
  render(pageContent(userNumber), container);
  return init();
};

const init = (): Promise<void> =>
  new Promise((resolve) => {
    const displayUserContinue = document.getElementById(
      "displayUserContinue"
    ) as HTMLButtonElement;
    displayUserContinue.onclick = () => resolve();
  });
