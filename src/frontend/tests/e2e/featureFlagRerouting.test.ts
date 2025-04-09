import { runInBrowser } from "./util";
import { II_URL } from "./constants";
import { NewAuthenticateView, AuthenticateView } from "./views";

const checkIfHasTailwind = async (browser: WebdriverIO.Browser) => {
  return await browser.execute(() => {
    const styles = Array.from(document.getElementsByTagName("style"));
    for (const style of styles) {
      if (style.innerText.includes("tailwindcss")) {
        return true;
      }
    }
    return false;
  });
};

test("Should redirect to new-styling authenticate with feature flag and load app.css", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Visit the root with feature flag
    await browser.url(`${II_URL}/?feature_flag_discoverable_passkey_flow=true`);

    // Check that we're redirected to authenticate page
    const authenticateView = new NewAuthenticateView(browser);
    await authenticateView.waitForDisplay();

    // Verify URL shows only "/"
    expect(await browser.getUrl()).toBe(`${II_URL}/`);

    // Check that app.css is loaded by verifying it's in the document
    const hasAppCss = await checkIfHasTailwind(browser);
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
    const hasAppCss = await checkIfHasTailwind(browser);
    expect(hasAppCss).toBe(false);
  });
}, 300_000);
