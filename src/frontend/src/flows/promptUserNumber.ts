import { html, render, TemplateResult } from "lit-html";
import { Ref, ref, createRef } from "lit-html/directives/ref.js";
import { mkAnchorInput } from "../components/anchorInput";

const promptUserNumberTemplate = ({
  title,
  userNumber,
  onContinue: onSubmit,
  onCancel,
}: {
  title: string;
  userNumber?: bigint;
  onContinue: (ret: bigint) => void;
  onCancel: () => void;
}): TemplateResult => {
  const userNumberContinue: Ref<HTMLButtonElement> = createRef();
  const anchorInput = mkAnchorInput({ userNumber, onSubmit });

  return html`
    <div class="l-container c-card c-card--highlight">
      <hgroup>
        <h1 class="t-title t-title--main">${title}</h1>
        <p class="t-lead">Please provide an Identity Anchor.</p>
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
      </div>
    </div>
  `;
};

export const promptUserNumberPage = (
  props: Parameters<typeof promptUserNumberTemplate>[0],
  container?: HTMLElement
): void => {
  const contain =
    container ?? (document.getElementById("pageContent") as HTMLElement);
  render(promptUserNumberTemplate(props), contain);
};

export const promptUserNumber = async ({
  title,
  userNumber,
}: {
  title: string;
  userNumber?: bigint;
}): Promise<bigint | "canceled"> =>
  new Promise((resolve) => {
    promptUserNumberPage({
      title,
      userNumber,
      onContinue: resolve,
      onCancel: () => resolve("canceled"),
    });
  });
