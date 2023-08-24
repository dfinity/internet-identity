import { toast } from "$src/components/toast";
import { withRef } from "$src/utils/lit-html";
import { Chan, withInputElement, zip } from "$src/utils/utils";
import { isNullish, nonNullish } from "@dfinity/utils";
import { TemplateResult, html } from "lit-html";
import { asyncReplace } from "lit-html/directives/async-replace.js";
import { Ref, createRef, ref } from "lit-html/directives/ref.js";

type Result<T> = { ok: true; value: T } | { ok: false; error: string };

// A Pin Input component
export const pinInput = <T>({
  pinLength = 6,
  onSubmit: onSubmit_,
  verify,
}: {
  pinLength?: number;
  onSubmit: (
    result: T
  ) => void /* Called when all inputs have been set or when .submit() is called */;
  verify: (
    pin: string
  ) => Promise<Result<T>> | Result<T> /* Used to verify & transform the pin */;
}): {
  template: TemplateResult;
  submit: () => void;
  hasError: Chan<boolean>;
  pinLength: number;
} => {
  // The root <ul> which lists the inputs
  const listRoot: Ref<HTMLUListElement> = createRef();

  // A currently displayed error, if any
  const currentError = new Chan<string | undefined>(undefined);
  const hasError = currentError.map((e) => nonNullish(e));

  // Called on submission, either autosubmit (when filled) or when '.submit()' is called
  const onSubmit: (pin: string) => void = async (pin: string) => {
    const result_ = verify(pin);
    // Only await if necessary
    const result = result_ instanceof Promise ? await result_ : result_;
    if (!result.ok) {
      currentError.send(result.error);
      return;
    }

    onSubmit_(result.value);
  };

  // We autosubmit only once
  let didAutosubmit = false;
  const autosubmit = (pin: string) => {
    if (didAutosubmit) {
      return;
    }

    didAutosubmit = true;
    onSubmit(pin);
  };

  const errorMessage = currentError.map((currentError) =>
    nonNullish(currentError)
      ? html`<p class="t-centered t-error c-input--stack">${currentError}</p>`
      : undefined
  );
  const notFilledError = `Please fill in all ${pinLength} inputs`;

  const withInputs = <T>(f: (inputs: HTMLInputElement[]) => T): T | undefined =>
    withRef(listRoot, (ul) => {
      const inputs = ul.querySelectorAll("input");
      if (isNullish(inputs)) {
        // Unexpected error
        toast.error("Could not find any inputs");
        return;
      }

      return f(Array.from(inputs));
    });

  const onInput_ = (event: InputEvent, element: HTMLInputElement) =>
    withInputs((inputs) => {
      currentError.send(undefined);
      onInput({ element, event, inputs, onFilled: autosubmit });
    });
  const onPaste_ = (event: ClipboardEvent, element: HTMLInputElement) =>
    withInputs((inputs) => {
      currentError.send(undefined);
      onPaste({ event, element, inputs, onFilled: onSubmit });
    });

  // A function that can be called to trigger submission. If some digits are missing, then
  // we show and error message.
  const submit = () =>
    withInputs((inputs) =>
      submitIfFilled({
        inputs,
        onSubmit,
        onMissing: () => currentError.send(notFilledError),
      })
    );

  const template = html`
    <div class="t-centered">
      <ul
        ${ref(listRoot)}
        class="c-list--pin"
        data-haserror=${asyncReplace(hasError)}
      >
        ${Array.from(
          { length: pinLength },
          (_, _ix) => html`
            <li class="c-list--pin-char c-input--anchor">
              <label class="c-input--anchor__wrap">
                <input
                  autocomplete="off"
                  type="tel"
                  size="1"
                  maxlength="1"
                  class="c-input c-input--pin c-input--pin__error"
                  @input=${(e: InputEvent) => withInputElement(e, onInput_)}
                  @paste=${(e: ClipboardEvent) => withInputElement(e, onPaste_)}
                  @focus=${(e: Event) =>
                    /* on focus, select the input so that the content is effectively replaced */ withInputElement(
                      e,
                      (_, elem) => elem.select()
                    )}
                />
              </label>
            </li>
          `
        )}
      </ul>
      ${asyncReplace(errorMessage)}
    </div>
  `;

  return { template, submit, hasError, pinLength };
};

/* Callback used when an input is being set */
const onInput = ({
  element,
  event,
  inputs,
  onFilled,
}: {
  element: HTMLInputElement;
  event: InputEvent;
  inputs: HTMLInputElement[];
  onFilled: (pin: string) => void;
}) => {
  // First, if the input is empty (i.e. the "input" was to delete the content OR the browser prevented inserting an extra char due to maxlength=1) then we do nothing
  // XXX: we don't test this because mocking the event data is really hard in jsdom
  if (event.data === "") {
    return;
  }

  // If the content was just deleted, then we just stay here
  if (isNullish(element.value) || element.value === "") {
    return;
  }

  // Only keep the last character (for instance if user focused & appended)
  // element.value = element.value.slice(-1);

  // Move focus to the next input, if any
  focusNextOrBlur(element);

  // Check if we should submit the content
  submitIfFilled({ inputs, onSubmit: onFilled });
};

/* Callback used when the user pastes */
const onPaste = ({
  event,
  inputs,
  element,
  onFilled,
}: {
  event: ClipboardEvent;
  element: HTMLInputElement;
  inputs: HTMLInputElement[];
  onFilled: (pin: string) => void;
}) => {
  // If no actual data was pasted, do nothing
  if (event.clipboardData === null) {
    return;
  }

  // Get a hold of all input elements that may be pasted into (this + everything afterwards)
  //  pasted ━┓
  // [] [] [] [] [] []
  //          ┗━━╋━━┛
  //          interesting
  const nextInputs = inputs.slice(inputs.indexOf(element));
  if (isNullish(nextInputs)) {
    toast.error("Could not find any input to paste into");
    return;
  }

  // Create an array of pairs of inputs + the char/digit to be pasted in that input
  const toBePasted = zip(
    nextInputs,
    event.clipboardData.getData("text").trim().split("")
  );
  if (toBePasted.length === 0) {
    return;
  }

  // Set the values manually
  for (const [input, char] of toBePasted) {
    input.value = char;
  }
  // Prevent actually pasting the value in any of the fields since we just did this manually
  event.preventDefault();

  if (toBePasted.length < nextInputs.length) {
    // If all inputs have NOT been filled, then focus on the one after the last paste/fill
    nextInputs[toBePasted.length].focus();
  } else {
    // otherwise, drop focus entirely
    element.blur();
  }

  // Check if we should submit the content
  submitIfFilled({ inputs, onSubmit: onFilled });
};

// Calls 'onSubmit', only if all inputs have a value. Otherwise, 'onMissing' is called.
const submitIfFilled = ({
  inputs,
  onSubmit,
  onMissing = () => {},
}: {
  inputs: HTMLInputElement[];
  onSubmit: (pin: string) => void;
  onMissing?: () => void;
}) => {
  const values = inputs.map((x) => x.value);

  if (values.every((value) => value !== "")) {
    onSubmit(values.join(""));
  } else {
    onMissing?.();
  }
};

// Focus the next input in the list, if any
const focusNextOrBlur = (element: HTMLInputElement) => {
  // Go up until we find a list item, then to the next sibling, and finally back down until we find an input
  const next = element
    .closest("li")
    ?.nextElementSibling?.querySelector("input");

  if (nonNullish(next)) {
    next.focus();
  } else {
    element.blur();
  }
};
