import { html, render } from "lit-html";
import {warningIcon} from "../components/icons";

const pageContent = (userNumber: bigint) => html`
  <div class="container">
    <h1>Congratulations!</h1>
    <p>
     Your new Identity Anchor has been created. You will need it later to use
     Internet Identity or to add additional devices.
    </p>
    <div class="nagBox">
      <div class="nagIcon">${warningIcon}</div>
      <div class="nagContent">
        <div class="nagTitle">Record Your Identity Anchor</div>
        <div class="nagMessage">
          Do <b>NOT</b> forget to save this Identity Anchor. Save a backup on a
          storage medium and write it down.</br>
          Note: It is advised to keep Identity Anchors secret in order to stay anonymous.
        </div>
      </div>
    </div>
    <label>Identity Anchor:</label>
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
