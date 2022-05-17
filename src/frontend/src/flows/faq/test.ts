import { questions } from "./index";

test("faq anchors are unique", () => {
  const anchors = Object.values(questions).map((q) => q.anchor);
  expect(anchors.sort()).toStrictEqual(Array.from(new Set(anchors)));
});
