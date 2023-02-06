import { html, TemplateResult } from "lit-html";
import { withRef, mount } from "@utils/lit-html";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { parseUserNumber } from "@utils/userNumber";

/** A component for inputting an anchor number */
export const mkAnchorInput = ({
  userNumber,
  onSubmit,
}: {
  userNumber?: bigint;
  onSubmit: (userNumber: bigint) => void;
}): {
  template: TemplateResult;
  userNumberInput: Ref<HTMLInputElement>;
  submit: () => void;
  readUserNumber: () => bigint | undefined;
} => {
  const divRef: Ref<HTMLDivElement> = createRef();
  const userNumberInput: Ref<HTMLInputElement> = createRef();

  const showHint = (message: string) => {
    withRef(divRef, (div) => {
      const error = "c-input__error--errored";
      const hide = "is-hidden";
      if (!div.classList.contains(error)) {
        div.classList.add(error);
        div.classList.remove(hide);
        div.textContent = message;
        setTimeout(() => {
          div.classList.remove(error);
          div.classList.add(hide);
        }, 2000);
      }
    });
  };

  // When "submitting" (either .submit() is called or enter is typed)
  // parse the value and call onSubmit
  const submit = () => {
    const result = readAndParseValue();
    if (result === "invalid") {
      return showHint("Invalid Anchor");
    }
    if (result === undefined) {
      return showHint("Please enter an Anchor");
    }
    onSubmit(result);
  };

  // How we react on unexpected (i.e. non-digit) input
  const onBadInput = () => {
    showHint("Anchors only consist of digits");
  };

  // When enter is pressed, submit
  const onKeyPress = (e: KeyboardEvent) => {
    if (e.key === "Enter") {
      e.preventDefault();
      submit();
    }
  };

  // Helper for reading the anchor/user number from the input
  const readAndParseValue = (): undefined | "invalid" | bigint => {
    return withRef(userNumberInput, (userNumberInput) => {
      const value = userNumberInput.value;
      if (value === "") {
        return undefined;
      }
      const parsed = parseUserNumber(value);
      if (parsed === null) {
        return "invalid";
      }
      return parsed;
    });
  };

  // Read user number if submit() shouldn't be called for some reason
  const readUserNumber = () => {
    const result = readAndParseValue();
    if (result === "invalid") {
      return undefined;
    }
    return result;
  };

  // Select the input when the component is rendered
  const selectInput = (elem: Element): void => {
    if (elem instanceof HTMLInputElement) {
      elem.select();
    }
  };

  const template = html` <div class="c-input--anchor l-stack">
    <label class="c-input--anchor__wrap" aria-label="Identity Anchor">
      <input
        ${ref(userNumberInput)}
        ${mount(selectInput)}
        type="text"
        data-role="anchor-input"
        class="c-input c-input--vip c-input--centered c-input--spacious"
        placeholder="Enter anchor"
        value="${ifDefined(userNumber?.toString())}"
        @input=${inputFilter(isDigits, onBadInput)}
        @keydown=${inputFilter(isDigits, onBadInput)}
        @keyup=${inputFilter(isDigits, onBadInput)}
        @mousedown=${inputFilter(isDigits, onBadInput)}
        @mouseup=${inputFilter(isDigits, onBadInput)}
        @select=${inputFilter(isDigits, onBadInput)}
        @contextmenu=${inputFilter(isDigits, onBadInput)}
        @drop=${inputFilter(isDigits, onBadInput)}
        @focusout=${inputFilter(isDigits, onBadInput)}
        @keypress=${onKeyPress}
      />
      <div ${ref(divRef)} class="c-card c-input__error is-hidden"></div>
    </label>
  </div>`;

  return { template, userNumberInput, submit, readUserNumber };
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
