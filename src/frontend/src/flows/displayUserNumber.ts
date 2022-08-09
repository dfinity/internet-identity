import { html, render } from "lit-html";
import { warningIcon } from "../components/icons";

const pageContent = (userNumber: bigint) => html`
  <article class="l-container c-card c-card--highlight">
    <hgroup>
      <h1 class="t-title t-title--main">Congratulations!</h1>
      <p class="t-lead">Your new Identity Anchor has been created.</p>
    </hgroup>
    <div class="c-card c-card--warning c-card--icon">
      <div class="c-card__icon">${warningIcon}</div>
      <div class="c-card__content">
        <div class="t-title">Record Your Identity Anchor</div>
        <p class="t-paragraph">
          Please record your new Identity Anchor. Keep a backup on a storage
          medium and write it down. You will need it later to use Internet
          Identity or to add additional devices. If you lose your Identity
          Anchor, you will no longer be able to use this identity to
          authenticate to dApps.
        </p>
      </div>
    </div>
    <div class="l-section">
      <h2 class="t-title">Identity Anchor</h2>
      <data class="c-card c-card--narrow c-card--outline t-vip"
        >${userNumber}</data
      >
      <button id="displayUserContinue" class="c-button">Continue</button>
    </div>
  </article>
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
