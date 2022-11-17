import { mkAnchorPicker } from "./anchorPicker";
import { render } from "lit-html";

const commonProps = {
  button: "go",
  recoverAnchor: () => {},
  register: () => {},
  addDevice: () => {},
};

test("first anchor is focused", async () => {
  const picker = mkAnchorPicker({
    savedAnchors: [BigInt(10000), BigInt(9990042)],
    pick: () => {},
    ...commonProps,
  });
  render(picker.template, document.body);
  // Tick once, otherwise element isn't focused yet
  await tick();
  const elem = document.activeElement as HTMLElement;
  expect(elem.dataset.anchorId).toBe("10000");
});

test("pick saved anchor", async () => {
  let picked: bigint | undefined = undefined;
  const picker = mkAnchorPicker({
    savedAnchors: [BigInt(10000), BigInt(9990042)],
    pick: (anchor) => {
      picked = anchor;
    },
    ...commonProps,
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

test("pick custom anchor", async () => {
  let picked: bigint | undefined = undefined;
  const picker = mkAnchorPicker({
    savedAnchors: [BigInt(10000), BigInt(9990042)],
    pick: (anchor) => {
      picked = anchor;
    },
    ...commonProps,
  });
  render(picker.template, document.body);
  // Tick once, otherwise element isn't focused yet
  await tick();
  const elem = document.querySelector(
    '[data-role="more-options"]'
  ) as HTMLElement;
  elem.click();
  await tick();
  const input = document.querySelector(
    '[data-role="anchor-input"]'
  ) as HTMLInputElement;
  input.value = "1234";

  const create = document.querySelector(
    '[data-role="anchor-create"]'
  ) as HTMLInputElement;
  create.click();

  expect(picked).toBe(BigInt(1234));
});

const tick = (): Promise<void> => new Promise((resolve) => setTimeout(resolve));
