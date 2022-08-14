import { html, render } from "lit-html";
import { warnBox } from "../components/warnBox";

const pageContent = (userNumber: bigint) => html`
  <div class="container">
    <h1>Congratulations!</h1>
    <p>Your new Identity Anchor has been created.</p>
    ${warnBox({
      title: "Record Your Identity Anchor",
      message:
        "Please record your new Identity Anchor. Keep a backup on a storage medium and write it down. You will need it later to use Internet Identity or to add additional devices. If you lose your Identity Anchor, you will no longer be able to use this identity to authenticate to dApps.",
      componentType: "div",
    })}
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
