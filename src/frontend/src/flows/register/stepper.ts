import { checkmarkIcon } from "$src/components/icons";
import { html } from "lit-html";

export const registerStepper = ({
  current,
}: {
  current: "create" | "captcha" | "finish";
}) => html`
  <div class="c-progress-container">
    <div class="c-progress-stepper">
      <div class="c-progress-step" aria-current=${current === "create"}>
        <span>1</span><span class="haha t-weak">Create Passkey</span>
      </div>
      <div class="c-progress-divider"></div>
      <div class="c-progress-step" aria-current=${current === "captcha"}>
        <span>2</span><span class="haha t-weak">Complete CAPTCHA</span>
      </div>
      <div class="c-progress-divider"></div>
      <div class="c-progress-step" aria-current=${current === "finish"}>
        <span><i class="c-progress-checkmark-icon">${checkmarkIcon}</i></span
        ><span class="haha t-weak">Get Internet Identity</span>
      </div>
    </div>
  </div>
`;
