import { confirmSeedPhrasePage, wordTemplate } from "./confirmSeedPhrase";
import { ref, createRef, Ref } from "lit-html/directives/ref.js";
import { render, html } from "lit-html";
import { I18n } from "../../utils/i18n";

const i18n = new I18n("en");

test("word changes state", async () => {
  const template = wordTemplate({
    word: { check: true, elem: createRef(), word: "hello" },
    update: () => {
      /* */
    },
    i: 0,
  });

  render(template, document.body);

  // Ticks are needed throughout the test for the Chan to kick off
  await tick();

  // By default, ensure the state is "pending" and expected word is correct
  const elem = document.querySelector(
    '[data-state="pending"]'
  ) as HTMLInputElement;
  expect(elem.dataset.expected).toBe("hello");

  // On input, make sure the state is still pending
  elem.value = "hell";
  elem.dispatchEvent(new Event("input"));
  await tick();
  expect(elem.dataset.state).toBe("pending");

  // When exiting (i.e. onchange) make sure the state is explicitly "incorrect"
  elem.dispatchEvent(new Event("change"));
  await tick();
  expect(elem.dataset.state).toBe("incorrect");

  // On correct input make sure the state is explicitly "correct"
  elem.value = "hello";
  elem.dispatchEvent(new Event("input"));
  await tick();
  expect(elem.dataset.state).toBe("correct");
});

test("words can be completed", async () => {
  const words = [
    { word: "one", check: false },
    { word: "two", check: true },
    { word: "three", check: false },
    { word: "four", check: true },
  ];

  await confirmSeedPhrasePage(
    {
      words,
      confirm: () => {
        /* */
      },
      back: () => {
        /* */
      },
      i18n,
    },
    document.body
  );

  await tick();

  const nextButton = document.querySelector(
    '[data-action="next"]'
  ) as HTMLButtonElement;

  expect(nextButton.disabled).toBe(true);

  const inputTwo = document.querySelector(
    '[data-expected="two"]'
  ) as HTMLInputElement;
  inputTwo.value = "two";
  inputTwo.dispatchEvent(new Event("input"));
  await tick();
  expect(nextButton.disabled).toBe(true);

  const inputFour = document.querySelector(
    '[data-expected="four"]'
  ) as HTMLInputElement;
  inputFour.value = "bad";
  inputFour.dispatchEvent(new Event("input"));
  await tick();
  expect(nextButton.disabled).toBe(true);

  inputFour.value = "four";
  inputFour.dispatchEvent(new Event("input"));
  await tick();
  expect(nextButton.disabled).toBe(false);
});

const tick = (): Promise<void> => new Promise((resolve) => setTimeout(resolve));
