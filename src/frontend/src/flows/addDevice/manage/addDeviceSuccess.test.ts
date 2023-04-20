import { I18n } from "../../../i18n";
import { addDeviceSuccessPage } from "./addDeviceSuccess";

describe("addDeviceSuccess", () => {
  it("should resolve promise on click to continue", () => {
    const onContinue = jest.fn();

    addDeviceSuccessPage({
      i18n: new I18n(),
      deviceAlias: "Test device alias",
      onContinue,
    });

    document.querySelector<HTMLButtonElement>('[data-action="next"]')?.click();

    expect(onContinue).toHaveBeenCalledTimes(1);
  });
});
