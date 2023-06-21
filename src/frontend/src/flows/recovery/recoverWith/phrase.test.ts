import { vi } from "vitest";
import { recoverWithPhrasePage } from "./phrase";

test("user number is set", () => {
  const f = vi.fn();

  recoverWithPhrasePage(
    {
      confirm: () => {
        /**/
      },
      back: () => {
        /**/
      },
      verify: ({ userNumber }) => {
        f(userNumber);
        return Promise.resolve({ tag: "err", message: "this is a test" });
      },
      message: "",
    },
    document.body
  );

  document.querySelectorAll("input")[0].value = "12345";
  (document.querySelector('[data-action="next"]') as HTMLButtonElement).click();
  expect(f).toHaveBeenCalledWith(BigInt(12345));
});
