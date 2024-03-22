import { I18n } from "$src/i18n";
import { vi } from "vitest";
import { addDeviceSuccessPage } from "./addDeviceSuccess";

describe("addDeviceSuccess", () => {
  it("should resolve promise on click to continue", () => {
    const onContinue = vi.fn();

    addDeviceSuccessPage(
      {
        i18n: new I18n(),
        deviceAlias: "Test device alias",
        onContinue,
      },
      document.body
    );

    document.querySelector<HTMLButtonElement>('[data-action="next"]')?.click();

    expect(onContinue).toHaveBeenCalledTimes(1);
  });
});
