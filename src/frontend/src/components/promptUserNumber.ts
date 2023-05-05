import { renderPage } from "$src/utils/lit-html";
import { html, TemplateResult } from "lit-html";
import { createRef, Ref, ref } from "lit-html/directives/ref.js";
import { mkAnchorInput } from "./anchorInput";
import { mainWindow } from "./mainWindow";

const promptUserNumberTemplate = ({
  title,
  message,
  userNumber,
  onContinue: onSubmit,
  onCancel,
}: {
  title: string;
  message?: string;
  userNumber?: bigint;
  onContinue: (ret: bigint) => void;
  onCancel: () => void;
}): TemplateResult => {
  const userNumberContinue: Ref<HTMLButtonElement> = createRef();
  const anchorInput = mkAnchorInput({ userNumber, onSubmit });

  const defaultMessage = "Please provide an Internet Identity.";
  const promptUserNumberSlot = html` <hgroup>
      <h1 class="t-title t-title--main">${title}</h1>
      <p class="t-lead">${message ?? defaultMessage}</p>
    </hgroup>
    ${anchorInput.template}
    <div class="c-button-group">
      <button
        @click="${onCancel}"
        id="userNumberCancel"
        class="c-button c-button--secondary"
      >
        Cancel
      </button>
      <button
        ${ref(userNumberContinue)}
        @click="${anchorInput.submit}"
        id="userNumberContinue"
        class="c-button"
      >
        Continue
      </button>
    </div>`;

  return mainWindow({
    showFooter: false,
    showLogo: false,
    slot: promptUserNumberSlot,
  });
};

export const promptUserNumberPage = renderPage(promptUserNumberTemplate);

export const promptUserNumber = ({
  title,
  message,
  userNumber,
}: {
  title: string;
  message?: string;
  userNumber?: bigint;
}): Promise<bigint | "canceled"> =>
  new Promise((resolve) => {
    promptUserNumberPage({
      title,
      message,
      userNumber,
      onContinue: resolve,
      onCancel: () => resolve("canceled"),
    });
  });
