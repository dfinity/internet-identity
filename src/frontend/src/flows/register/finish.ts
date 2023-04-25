import { checkmarkIcon, copyIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
import { warnBox } from "$src/components/warnBox";
import { renderPage, withRef } from "$src/utils/lit-html";
import { html } from "lit-html";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";

export const displayUserNumberTemplate = ({
  onContinue,
  userNumber,
}: {
  onContinue: () => void;
  userNumber: bigint;
}) => {
  const userNumberCopy: Ref<HTMLButtonElement> = createRef();

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
    <output class="c-input c-input--textarea c-input--readonly c-input--icon" >
      <div class="t-vip" aria-label="usernumber" id="userNumber" data-usernumber="${userNumber}">${userNumber}</div>
      <button
        ${ref(userNumberCopy)}
        aria-label="Copy phrase to clipboard""
        title="Copy phrase to clipboard"
        tabindex="0"
        class="c-button__icon"
        @click=${async () => {
          try {
            await navigator.clipboard.writeText(userNumber.toString());
            withRef(userNumberCopy, (elem) => {
              elem.classList.add("is-copied");
            });
          } catch (e: unknown) {
            toast.error("Unable to copy Identity Anchor");
            console.error("Unable to copy Identity Anchor", e);
          }
        }}
        >
          <span>Copy</span>
          ${copyIcon}
          ${checkmarkIcon}
        </button>
    </output>
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

export const displayUserNumberPage = renderPage(displayUserNumberTemplate);

export const displayUserNumber = (userNumber: bigint): Promise<void> => {
  return new Promise((resolve) =>
    displayUserNumberPage({ onContinue: () => resolve(), userNumber })
  );
};
