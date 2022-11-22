import { html, render } from "lit-html";
import { warnBox } from "../../components/warnBox";

const pageContent = (userNumber: bigint) => html`
  <div class="l-container c-card c-card--highlight">
    <hgroup>
      <h1 class="t-title t-title--main">Congratulations!</h1>
      <p class="t-paragraph">Your new Identity Anchor has been created.</p>
    </hgroup>
    ${warnBox({
      title: "Record Your Identity Anchor",
      message:
        "Please record your new Identity Anchor. Keep a backup on a storage medium and write it down. You will need it later to use Internet Identity or to add additional devices. If you lose your Identity Anchor, you will no longer be able to use this identity to authenticate to dApps.",
    })}
    <h2 class="t-title">Identity Anchor:</h2>

    <div class="c-input c-input--vip c-input--readonly t-vip" data-usernumber>
      ${userNumber}
    </div>
    <div class="l-stack">
      <button id="displayUserContinue" class="c-button">Continue</button>
    </div>
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
