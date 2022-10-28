import { html, TemplateResult } from "lit-html";
import { withRef } from "../utils/utils";
import { createRef, ref, Ref } from "lit-html/directives/ref.js";
import { ifDefined } from "lit-html/directives/if-defined.js";
import { DirectiveResult } from "lit-html/directive.js";
import { parseUserNumber } from "../utils/userNumber";

/** A component for inputting an anchor number */
export const mkAnchorInput = ({
  inputId,
  userNumber,
  onSubmit,
  focus = true,
}: {
  inputId: string;
  userNumber?: bigint;
  onSubmit?: (userNumber: bigint) => void;
  focus?: boolean;
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
    onSubmit?.(result);
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

  const template = html` <div class="c-input--anchor">
    <label class="c-input--anchor__wrap" aria-label="Identity Anchor">
      <input
        ${ref(userNumberInput)}
        ${focus ? mount(selectInput) : ""}
        type="text"
        id="${inputId}"
        class="c-input c-input--vip"
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

/* A lit-html directive that performs an action when the element is added
 * to the DOM.
 *
 * Note: there are no guarantees that the callback will be called as the
 * implementation relies on internal "lit-html" behavior. No critical behavior
 * should depend on this.
 */
const mount = (callback: (elem: Element) => void): DirectiveResult =>
  ref((e: Element | undefined) => {
    if (e !== undefined) {
      // This works by observing the entire document for mutations, under
      // the assumption that the first DOM mutation to happen after the element
      // was created is inserting the element (or its parent) in the DOM. This
      // happens in practice but may change depending on what lit-html does.
      //
      // Note: The reason why we only observe exactly one mutation and then
      // disconnect is to avoid leaking the observer to keep observing the
      // DOM if the element was created but never mounted.
      //
      // Note: it would be much easier to use the "DOMNodeInsertedIntoDocument"
      // event on the element itself, but the API is deprecated and Firefox
      // does not support it.
      const observer = new MutationObserver(() => {
        try {
          // check that the element is indeed in the DOM and call callback
          if (e.isConnected) {
            callback(e);
          }
        } finally {
          observer.disconnect();
        }
      });

      observer.observe(document, { childList: true, subtree: true });
    }
  });
