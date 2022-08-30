import { html, render } from "lit-html";
import { parseUserNumber } from "../utils/userNumber";

const pageContent = (title: string, userNumber: bigint | null) => html`
  <div class="l-container c-card c-card--highlight">
    <hgroup>
      <h1 class="t-title t-title--main">${title}</h1>
      <p class="t-lead">Please provide an Identity Anchor.</p>
    </hgroup>
    <input
      type="text"
      id="userNumberInput"
      class="c-input c-input--vip"
      placeholder="Enter Identity Anchor"
      value=${userNumber ?? ""}
    />
    <button id="userNumberContinue" class="c-button c-button--primary">
      Continue
    </button>
    <button id="userNumberCancel" class="c-button">Cancel</button>
  </div>
`;

export const promptUserNumber = async (
  title: string,
  userNumber: bigint | null
): Promise<bigint | null> =>
  new Promise((resolve) => {
    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(title, userNumber), container);

    const userNumberContinue = document.getElementById(
      "userNumberContinue"
    ) as HTMLButtonElement;
    const userNumberInput = document.getElementById(
      "userNumberInput"
    ) as HTMLInputElement;

    userNumberInput.onkeypress = (e) => {
      // submit if user hits enter
      if (e.key === "Enter") {
        e.preventDefault();
        userNumberContinue.click();
      }
    };

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
