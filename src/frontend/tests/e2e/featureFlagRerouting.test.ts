import { runInBrowser } from "./util";
import { II_URL } from "./constants";
import { NewAuthenticateView, AuthenticateView } from "./views";
import { ElementArray } from "webdriverio";

const checkIfHasTailwind = async (styles: ElementArray) => {
  for (const style of styles) {
    const text = await style.getElementText(style.elementId);
    if (await text.includes("tailwindcss")) {
      return true;
    }
  }
  return false;
};

test("Should redirect to new-styling authenticate with feature flag and load app.css", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Visit the root with feature flag
    await browser.url(`${II_URL}/?feature_flag_discoverable_passkey_flow=true`);

    // Check that we're redirected to authenticate page
    const newAuthenticateView = new NewAuthenticateView(browser);
    await newAuthenticateView.waitForDisplay();

    // Verify URL shows only "/"
    expect(await browser.getUrl()).toBe(`${II_URL}/`);

    // Check that app.css is loaded by verifying it's in the document
    const hasAppCss = checkIfHasTailwind(
      await newAuthenticateView.getStyleSheets(),
    );
    expect(hasAppCss).toBe(true);
  });
}, 300_000);

test("Should show regular view without feature flag and not load app.css", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Visit the root without feature flag
    await browser.url(II_URL);

    // Check that we're on the regular page (not authenticate)
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();

    // Check that app.css is loaded by verifying it's in the document
    const hasAppCss = await checkIfHasTailwind(
      await authenticateView.getStyleSheets(),
    );
    expect(hasAppCss).toBe(false);
  });
}, 300_000);
