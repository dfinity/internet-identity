import { html, render } from "lit-html";
import { warnBox } from "../../components/warnBox";
import { mainWindow } from "../../components/mainWindow";

export const displayUserNumberTemplate = ({
  onContinue,
  userNumber,
}: {
  onContinue: () => void;
  userNumber: bigint;
}) => {
  const displayUserNumberSlot = html`<hgroup>
      <h1 class="t-title t-title--main">Congratulations!</h1>
      <p class="t-paragraph">Your new Identity Anchor has been created.</p>
    </hgroup>
    ${warnBox({
      additionalClasses: ["l-stack"],
      title: "Record Your Identity Anchor",
      message:
        "Please record your new Identity Anchor. Keep a backup on a storage medium and write it down. You will need it later to use Internet Identity or to add additional devices. If you lose your Identity Anchor, you will no longer be able to use this identity to authenticate to dApps.",
    })}
    <h2 class="t-title">Identity Anchor:</h2>

    <div class="c-input c-input--vip c-input--readonly t-vip" data-usernumber>
      ${userNumber}
    </div>
    <div class="l-stack">
      <button
        @click=${() => onContinue()}
        id="displayUserContinue"
        class="c-button"
      >
        Continue
      </button>
    </div> `;

  return mainWindow({
    showLogo: false,
    showFooter: false,
    slot: displayUserNumberSlot,
  });
};

export const displayUserNumberPage = (
  props: Parameters<typeof displayUserNumberTemplate>[0],
  container?: HTMLElement
): void => {
  const contain =
    container ?? (document.getElementById("pageContent") as HTMLElement);
  render(displayUserNumberTemplate(props), contain);
};

export const displayUserNumber = async (userNumber: bigint): Promise<void> => {
  return new Promise((resolve) =>
    displayUserNumberPage({ onContinue: () => resolve(), userNumber })
  );
};
