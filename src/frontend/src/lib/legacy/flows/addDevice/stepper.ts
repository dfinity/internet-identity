import { checkmarkIcon } from "$lib/templates/icons";
import { html } from "lit-html";

export const tentativeDeviceStepper = ({
  step,
}: {
  step: "activate" | "verify" | "success";
}) => html`
  <div class="c-progress-container">
    <ol class="c-progress-stepper">
      <li class="c-progress-stepper__step" aria-current=${step === "activate"}>
        <span class="c-progress-stepper__label">Activate Passkey</span>
      </li>
      <li class="c-progress-stepper__step" aria-current=${step === "verify"}>
        <span class="c-progress-stepper__label">Verify Device</span>
      </li>
      <li
        class="c-progress-stepper__step c-progress-stepper__step--final"
        aria-current=${step === "success"}
      >
        <i class="c-progress-stepper__icon">${checkmarkIcon}</i>
        <span class="c-progress-stepper__label">Passkey Activated</span>
      </li>
    </ol>
  </div>
`;
