import { renderAddDeviceSuccess } from "./addDeviceSuccess";

describe("addDeviceSuccess", () => {
  const deviceAlias = "Test device alias";

  beforeAll(() => {
    const pageContent = document.createElement("div");
    pageContent.setAttribute("id", "pageContent");
    document.body.appendChild(pageContent);
  });

  it("should resolve promise on click to continue", async () => {
    const result = renderAddDeviceSuccess({
      deviceAlias,
    });

    document
      .querySelector<HTMLButtonElement>('[data-action="continueToHome"]')
      ?.click();

    await result;
  });
});
