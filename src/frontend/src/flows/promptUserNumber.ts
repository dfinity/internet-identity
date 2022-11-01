import { html, render, TemplateResult } from "lit-html";
import { Ref, ref, createRef } from "lit-html/directives/ref.js";
import { mkAnchorInput } from "../components/anchorInput";

const pageContent = (
  title: string,
  userNumber: bigint | null,
  callbacks: { onContinue: (ret: bigint) => void; onCancel: () => void }
): { template: TemplateResult } => {
  const userNumberContinue: Ref<HTMLButtonElement> = createRef();
  const anchorInput = mkAnchorInput({
    userNumber: userNumber ?? undefined,
    onSubmit: (userNumber: bigint) => {
      callbacks.onContinue(userNumber);
    },
  });

  const template = html`
    <div class="l-container c-card c-card--highlight">
      <hgroup>
        <h1 class="t-title t-title--main">${title}</h1>
        <p class="t-lead">Please provide an Identity Anchor.</p>
      </hgroup>
      ${anchorInput.template}
      <div class="c-button-group">
        <button
          @click="${callbacks.onCancel}"
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

  return { template };
};

export const promptUserNumber = async (
  title: string,
  userNumber: bigint | null
): Promise<bigint | null> =>
  new Promise((resolve) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    const content = pageContent(title, userNumber, {
      onContinue: resolve,
      onCancel: () => resolve(null),
    });
    render(content.template, container);
  });
