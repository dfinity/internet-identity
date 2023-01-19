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
      <h1 class="t-title t-title--main">
        You successfully created your Identity Anchor!
      </h1>
      <p class="t-paragraph">
        Use your Identity Anchor to create independent accounts with dapps on
        the Internet Computer.
      </p>
    </hgroup>
    <h2 class="t-title">Identity Anchor:</h2>
    <div class="c-input c-input--vip c-input--readonly t-vip" data-usernumber>
      ${userNumber}
    </div>
    ${warnBox({
      additionalClasses: ["l-stack"],
      title: "Write this number down",
      message:
        "If you lose this number, you lose access to your Internet Identity.",
    })}
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
