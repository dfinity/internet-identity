import { render } from "lit-html";
import { pinInput } from "./pinInput";

// A pin input where the verification step is successful and returns the pin
const pinInputId = (opts: { onSubmit: (pin: string) => void }) =>
  pinInput({
    ...opts,
    verify: (pin: string) => ({ ok: true, value: pin }),
  });

// Dispatch an "InputEvent"-ish event that works in jsdom
function dispatchInput(elem: HTMLInputElement, text: string) {
  elem.value = text;
  elem.dispatchEvent(new Event("input"));
}

// Dispatch an "ClipboardEvent"-ish event that works in jsdom
function dispatchPaste(elem: HTMLInputElement, text: string) {
  const evnt: any = new Event("paste", { bubbles: true, cancelable: true });
  evnt.clipboardData = {
    getData() {
      return text;
    },
  };
  elem.dispatchEvent(evnt);
}

// Get all inputs on the page
function getAllInputs(): HTMLInputElement[] {
  return Array.from(document.body.querySelectorAll("input"));
}

// A noop that makes eslint happy
export const noop = () => {
  /* */
};

describe("pin input", () => {
  // Ensure the DOM is clean before running the tests
  beforeEach(() => {
    document.getElementsByTagName("html")[0].innerHTML = "";
  });

  test("writing to all chars triggers onSubmit", () => {
    const onSubmit: (pin: string) => void = vi.fn();
    const pinInput_ = pinInputId({
      onSubmit,
    });

    render(pinInput_.template, document.body);

    for (const input of getAllInputs()) {
      dispatchInput(input, "a");
    }

    expect(onSubmit).toHaveBeenCalledWith(
      Array(pinInput_.pinLength).fill("a").join("")
    );
  });

  test("writing to all chars triggers submit on every dispatch", () => {
    const onSubmit: (pin: string) => void = vi.fn();

    render(pinInputId({ onSubmit }).template, document.body);

    const inputs = getAllInputs();

    for (const input of inputs) {
      dispatchInput(input, "a");
    }

    expect(onSubmit).toHaveBeenCalledOnce();
    dispatchInput(inputs[0], "a"); // after an extra dispatch the value should be submitted again
    expect(onSubmit).toHaveBeenCalledTimes(2);
  });

  test("writing to all chars removes focus", () => {
    render(pinInputId({ onSubmit: noop }).template, document.body);

    for (const input of getAllInputs()) {
      dispatchInput(input, "a");
    }

    const noneHasFocus = getAllInputs().every(
      (input) => input !== document.activeElement
    );

    expect(noneHasFocus).toBeTruthy();
  });

  test("input moves focus to next", () => {
    render(
      pinInputId({
        onSubmit: () => {},
      }).template,
      document.body
    );

    const [first, second] = getAllInputs();

    first.focus();

    expect(first).toBe(document.activeElement);
    expect(second).not.toBe(document.activeElement);

    dispatchInput(first, "a");

    expect(first).not.toBe(document.activeElement);
    expect(second).toBe(document.activeElement);
  });

  test("deleting keeps focus", () => {
    render(
      pinInputId({
        onSubmit: noop,
      }).template,
      document.body
    );
    const input = getAllInputs()[2];
    dispatchInput(input, "a");
    input.focus();
    dispatchInput(input, "");

    expect(input).toBe(document.activeElement);
  });

  test("pasting triggers onSubmit", () => {
    const onSubmit: (pin: string) => void = vi.fn();
    const pinInput_ = pinInputId({ onSubmit });

    render(pinInput_.template, document.body);
    const pin = Array(pinInput_.pinLength).fill("a").join("");
    dispatchPaste(getAllInputs()[0], pin);

    expect(onSubmit).toHaveBeenCalledWith(pin);
  });

  test("pasting all removes focus", () => {
    const pinInput_ = pinInputId({
      onSubmit: noop,
    });
    render(pinInput_.template, document.body);
    const pin = Array(pinInput_.pinLength).fill("a").join("");
    dispatchPaste(getAllInputs()[0], pin);

    // Check that no input is focused
    const noneHasFocus = getAllInputs().every(
      (input) => input !== document.activeElement
    );
    expect(noneHasFocus).toBeTruthy();
  });

  test("pasting moves focus", () => {
    render(pinInputId({ onSubmit: noop }).template, document.body);
    const inputs = getAllInputs();
    // dispatch a paste that sits somewhere in the middle (not dispatched in the first input, and not long enough
    // to reach the last input)
    const content = "123";
    dispatchPaste(inputs[2], content);

    // Focus should have moved (3 = length of "123")
    expect(inputs[2 + content.length]).toBe(document.activeElement);
  });

  test("calling .submit() does submit", () => {
    const onSubmit: (pin: string) => void = vi.fn();
    const pinInput_ = pinInputId({ onSubmit });

    render(pinInput_.template, document.body);

    for (const input of getAllInputs()) {
      // we don't call dispatchInput on purpose to not trigger an auto submit
      input.value = "a";
    }

    expect(onSubmit).not.toHaveBeenCalled();
    pinInput_.submit();
    expect(onSubmit).toHaveBeenCalledOnce();
  });

  test("calling .submit() triggers error if not full", () => {
    const pinInput_ = pinInputId({
      onSubmit: noop,
    });

    render(pinInput_.template, document.body);

    const someInputs = getAllInputs();
    someInputs.shift();
    someInputs.shift();

    for (const input of someInputs) {
      dispatchInput(input, "a");
    }

    expect(pinInput_.hasError.latest).not.toBe(true);
    pinInput_.submit();
    expect(pinInput_.hasError.latest).toBe(true);
  });

  test("input removes error state", () => {
    const pinInput_ = pinInputId({
      onSubmit: noop,
    });

    render(pinInput_.template, document.body);

    // Submit on empty, to trigger error
    pinInput_.submit();

    expect(pinInput_.hasError.latest).toBe(true);
    dispatchInput(getAllInputs()[0], "a");
    expect(pinInput_.hasError.latest).not.toBe(true);
  });
});
