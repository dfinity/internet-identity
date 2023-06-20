import { render } from "lit-html";
import { mkAnchorPicker } from "./anchorPicker";

test("first anchor is focused", async () => {
  const picker = mkAnchorPicker({
    savedAnchors: [BigInt(10000), BigInt(9990042)],
    pick: () => {},
    moreOptions: () => {},
    focus: true,
  });
  render(picker.template, document.body);
  // jsdom does not follow the HTML spec for 'autofocus':
  //    https://github.com/jsdom/jsdom/issues/3041
  // so instead of checking for the element to be focused, we just check that
  // the attribute is present and trust the HTML spec.
  const elem = document.querySelector("[autofocus]") as HTMLElement;
  expect(elem.dataset.anchorId).toBe("10000");
});

test("pick saved anchor", async () => {
  let picked: bigint | undefined = undefined;
  const picker = mkAnchorPicker({
    savedAnchors: [BigInt(10000), BigInt(9990042)],
    pick: (anchor) => {
      picked = anchor;
    },
    moreOptions: () => {},
    focus: true,
  });
  render(picker.template, document.body);
  // Tick once, otherwise element isn't focused yet
  await tick();
  const elem = document.querySelector(
    '[data-anchor-id="10000"]'
  ) as HTMLElement;
  elem.click();
  expect(picked).toBe(BigInt(10000));
});

const tick = (): Promise<void> => new Promise((resolve) => setTimeout(resolve));
