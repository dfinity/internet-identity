import { html } from "lit";
import { TemplateRef, renderTemplateRef } from "../utils/templateRef";
import { parseUserNumber } from "../utils/userNumber";
import { mkAnchorInput } from "../components/anchorInput";

const pageContent = (
  title: string,
  userNumber: bigint | null
): TemplateRef<{ userNumberInput: HTMLInputElement }> => {
  const anchorInput = mkAnchorInput("userNumberInput", userNumber ?? undefined);
  const template = html`
    <div class="l-container c-card c-card--highlight">
      <hgroup>
        <h1 class="t-title t-title--main">${title}</h1>
        <p class="t-lead">Please provide an Identity Anchor.</p>
      </hgroup>
      ${anchorInput.template}
      <div class="c-button-group">
        <button id="userNumberCancel" class="c-button c-button--secondary">
          Cancel
        </button>
        <button id="userNumberContinue" class="c-button">Continue</button>
      </div>
    </div>
  `;

  return { ...anchorInput, template };
};

export const promptUserNumber = async (
  title: string,
  userNumber: bigint | null
): Promise<bigint | null> =>
  new Promise((resolve) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    const content = pageContent(title, userNumber);
    const { userNumberInput } = renderTemplateRef(content, container);

    const userNumberContinue = document.getElementById(
      "userNumberContinue"
    ) as HTMLButtonElement;

    userNumberInput.onkeypress = (e) => {
      // submit if user hits enter
      if (e.key === "Enter") {
        e.preventDefault();
        userNumberContinue.click();
      }
    };

    // always select the input
    userNumberInput.select();

    userNumberContinue.onclick = () => {
      const userNumber = parseUserNumber(userNumberInput.value);
      if (userNumber !== null) {
        resolve(userNumber);
      } else {
        userNumberInput.classList.toggle("has-error", true);
        userNumberInput.placeholder = "Please enter an Identity Anchor first";
      }
    };

    const userNumberCancel = document.getElementById(
      "userNumberCancel"
    ) as HTMLButtonElement;

    if (userNumberCancel !== null) {
      userNumberCancel.onclick = async () => {
        resolve(null);
      };
    }
  });
