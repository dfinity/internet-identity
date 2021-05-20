import { html, render } from "lit-html";
import { parseUserNumber } from "../utils/userNumber";

const pageContent = (title: string, userNumber: bigint | null) => html`
  <div class="container">
    <h1>${title}</h1>
    <p>Please provide your user number.</p>
    <input
      type="text"
      id="userNumberInput"
      placeholder="Enter User Number"
      value=${userNumber ?? ""}
    />
    <button id="userNumberContinue" class="primary">Continue</button>
  </div>
`;

// TODO: Let the user go back?
export const promptUserNumber = async (
  title: string,
  userNumber: bigint | null
): Promise<bigint> =>
  new Promise((resolve) => {
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
        userNumberInput.classList.toggle("errored", true);
        userNumberInput.placeholder = "Please enter your User Number first";
      }
    };

    const container = document.getElementById("pageContent") as HTMLElement;
    render(pageContent(title, userNumber), container);
  });
