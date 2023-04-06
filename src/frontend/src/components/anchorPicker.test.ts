import { render } from "lit-html";
import { mkAnchorPicker } from "./anchorPicker";

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
    moreOptions: () => {},
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
    moreOptions: () => {},
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

const tick = (): Promise<void> => new Promise((resolve) => setTimeout(resolve));
