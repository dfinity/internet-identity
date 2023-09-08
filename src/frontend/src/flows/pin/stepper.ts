import { checkmarkIcon } from "$src/components/icons";
import { html } from "lit-html";

export const pinStepper = ({
  current,
}: {
  current: "set" | "confirm" | "captcha" | "finish";
}) => html`
  <div class="c-progress-container">
    <ol class="c-progress-stepper">
      <li class="c-progress-stepper__step" aria-current=${current === "set"}>
        <span class="c-progress-stepper__label">Set PIN</span>
      </li>
      <li
        class="c-progress-stepper__step"
        aria-current=${current === "confirm"}
      >
        <span class="c-progress-stepper__label">Confirm PIN</span>
      </li>
      <li
        class="c-progress-stepper__step"
        aria-current=${current === "captcha"}
      >
        <span class="c-progress-stepper__label">Complete CAPTCHA</span>
      </li>
      <li
        class="c-progress-stepper__step c-progress-stepper__step--final"
        aria-current=${current === "finish"}
      >
        <i class="c-progress-stepper__icon">${checkmarkIcon}</i>
        <span class="c-progress-stepper__label">Get Internet Identity</span>
      </li>
    </ol>
  </div>
`;
