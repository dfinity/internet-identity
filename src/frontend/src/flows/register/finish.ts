import { checkmarkIcon, copyIcon } from "$src/components/icons";
import { mainWindow } from "$src/components/mainWindow";
import { toast } from "$src/components/toast";
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
        You’ve created an Internet Identity!
      </h1>
      <p class="t-paragraph">
        Save this number by taking a screenshot or writing it down.
      </p>
    </hgroup>
    <output class="c-input c-input--textarea c-input--readonly c-input--icon c-input--id" >
    <h2 class="t-title" style="margin-top: 0.5em; color: white; font-size: 1.2rem;">Internet Identity:</h2>
      <div style="color: white; text-align: left; font-size: 4rem;" class="t-vip" aria-label="usernumber" id="userNumber" data-usernumber="${userNumber}">${userNumber}</div>
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
      <button
        @click=${() => onContinue()}
        id="displayUserContinue"
        class="c-button l-stack"
      >
        I saved it, continue
      </button>
    <section class="c-marketing-block">
      <aside class="l-stack">
        <h3 class="t-title">What is this number?</h3>
        <ul class="c-list c-list--bulleted">
          <li>Your Internet Identity</li>
          <li>With your Internet Identity and your passkey, you will be able to create and sign into private accounts on IC dapps</li>
        </ul>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">Why is it important to save this number?</h3>
        <ul class="c-list c-list--bulleted">
          <li>If you lose this number, you will lose access to all of the accounts that you created with it</li>
        </ul>
      </aside>

      <aside class="l-stack">
        <h3 class="t-title">Is this number a secret?</h3>
        <ul class="c-list c-list--bulleted">
          <li>No, this number is unique to you but it is not a secret.</li>
        </ul>
      </aside>
    </section>

    `;

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
