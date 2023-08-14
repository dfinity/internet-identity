import { checkmarkIcon } from "$src/components/icons";
import { html } from "lit-html";

export const vcStepper = ({
  current,
}: {
  current: "prompt" | "select" | "finish";
}) => html`
  <div class="c-progress-container">
    <ol class="c-progress-stepper">
      <li
        class="c-progress-stepper__step"
        aria-current=${current === "prompt"}
      ></li>
      <li
        class="c-progress-stepper__step"
        aria-current=${current === "select"}
      ></li>
      <li
        class="c-progress-stepper__step c-progress-stepper__step--final"
        aria-current=${current === "finish"}
      >
        <i class="c-progress-stepper__icon">${checkmarkIcon}</i>
      </li>
    </ol>
  </div>
`;
