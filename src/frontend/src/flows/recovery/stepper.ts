import { checkmarkIcon } from "$src/components/icons";
import { html } from "lit-html";

export const phraseStepper = ({
  current,
}: {
  current: "store" | "confirm";
}) => html`
  <div class="c-progress-container">
    <ol class="c-progress-stepper c-progress-stepper--narrow">
      <li class="c-progress-stepper__step" aria-current=${current === "store"}>
        <span class="c-progress-stepper__label">Store Phrase</span>
      </li>
      <li
        class="c-progress-stepper__step c-progress-stepper__step--final"
        aria-current=${current === "confirm"}
      >
        <i class="c-progress-stepper__icon">${checkmarkIcon}</i>
        <span class="c-progress-stepper__label">Confirm Recovery</span>
      </li>
    </ol>
  </div>
`;
