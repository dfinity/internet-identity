import { renderAddDeviceSuccess } from "./addDeviceSuccess";
import copyJson from "./addDeviceSuccess.json";

describe("addDeviceSuccess", () => {
  const deviceAlias = "Test device alias";

  const { en } = copyJson;

  const waitFor = (callback: () => void): Promise<void> =>
    new Promise((resolve) => {
      const start = Date.now();

      const callCallback = () => {
        if (Date.now() > start + 10 * 1000) {
          resolve();
          return;
        }

        try {
          callback();
          resolve();
        } catch (err: unknown) {
          // Ignore error and try again in a bit
          setTimeout(callCallback, 100);
        }
      };

      callCallback();
    });

  beforeAll(() => {
    const pageContent = document.createElement("div");
    pageContent.setAttribute("id", "pageContent");
    document.body.appendChild(pageContent);
  });

  it("should render static content", async () => {
    renderAddDeviceSuccess({
      deviceAlias,
    });

    await waitFor(() => expect(document.body.textContent).toContain(en.title));
    expect(document.body.textContent).toContain(en.explore);
  });

  it("should render a call to action", () => {
    renderAddDeviceSuccess({
      deviceAlias,
    });

    expect(document.querySelector('[data-action="continueToHome"]')).not.toBeNull();
  });

  it("should resolve promise on click to continue", (done) => {
    renderAddDeviceSuccess({
      deviceAlias,
    }).then(() => done());

    document
      .querySelector<HTMLButtonElement>('[data-action="continueToHome"]')
      ?.click();
  });
});
