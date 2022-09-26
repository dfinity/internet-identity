import { html } from "lit";
import { createRef, ref } from "lit/directives/ref.js";
import { TemplateRef } from "../utils/templateRef";

/** A component for inputting an anchor number */
export const mkAnchorInput = (
  inputId: string,
  userNumber?: bigint
): TemplateRef<{ userNumberInput: HTMLInputElement }> => {
  const divRef = createRef();
  const userNumberInput = createRef();

  // How we react on unexpected (i.e. non-digit) input
  const onBadInput = () => {
    const div = divRef.value;
    if (div !== undefined && !div.classList.contains("flash-error")) {
      div.classList.add("flash-error");
      setTimeout(() => div.classList.remove("flash-error"), 2000);
    }
  };

  const template = html` <div ${ref(divRef)} class="l-stack c-input--anchor">
    <label class="c-input--anchor__wrap" aria-label="Identity Anchor">
      <input
        ${ref(userNumberInput)}
        type="text"
        id="${inputId}"
        class="c-input c-input--vip"
        placeholder="Enter anchor"
        value="${userNumber !== undefined ? userNumber : ""}"
        @input=${inputFilter(isDigits, onBadInput)}
        @keydown=${inputFilter(isDigits, onBadInput)}
        @keyup=${inputFilter(isDigits, onBadInput)}
        @mousedown=${inputFilter(isDigits, onBadInput)}
        @mouseup=${inputFilter(isDigits, onBadInput)}
        @select=${inputFilter(isDigits, onBadInput)}
        @contextmenu=${inputFilter(isDigits, onBadInput)}
        @drop=${inputFilter(isDigits, onBadInput)}
        @focusout=${inputFilter(isDigits, onBadInput)}
      />
    </label>

    <p
      id="invalidAnchorMessage"
      class="anchor-error-message is-hidden t-paragraph t-strong"
    >
      The Identity Anchor is not valid. Please try again.
    </p>
  </div>`;

  return { template, refs: { userNumberInput } };
};

const isDigits = (c: string) => /^\d*\.?\d*$/.test(c);

/* Adds a filter to the input that only allows the given regex.
 * For more info see https://stackoverflow.com/questions/469357/html-text-input-allow-only-numeric-input
 */
const inputFilter = (inputFilter: (c: string) => boolean, onBad: () => void) =>
  function (
    this: (HTMLInputElement | HTMLTextAreaElement) & {
      oldValue: string;
      oldSelectionStart: number | null;
      oldSelectionEnd: number | null;
    }
  ) {
    if (inputFilter(this.value)) {
      this.oldValue = this.value;
      this.oldSelectionStart = this.selectionStart;
      this.oldSelectionEnd = this.selectionEnd;
    } else {
      onBad();

      if (Object.prototype.hasOwnProperty.call(this, "oldValue")) {
        this.value = this.oldValue;
        if (this.oldSelectionStart !== null && this.oldSelectionEnd !== null) {
          this.setSelectionRange(this.oldSelectionStart, this.oldSelectionEnd);
        }
      } else {
        this.value = "";
      }
    }
  };
