import { runInBrowser } from "./util";
import { II_URL } from "./constants";
import { NewAuthenticateView, MainView } from "./views";

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
    const hasAppCss = await browser.execute(() => {
      const links = Array.from(document.getElementsByTagName("link"));
      for (const link of links) {
        if (link.href.includes("app.css")) {
          return true;
        }
      }
      return false;
    });
    expect(hasAppCss).toBe(true);
  });
}, 300_000);

test("Should show regular view without feature flag and not load app.css", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Visit the root without feature flag
    await browser.url(II_URL);

    // Check that we're on the regular page (not authenticate)
    const mainView = new MainView(browser);
    await mainView.waitForDisplay();

    // Check that app.css is not loaded
    const hasAppCss = await browser.execute(() => {
      const links = Array.from(document.getElementsByTagName("link"));
      for (const link of links) {
        if (link.href.includes("app.css")) {
          return true;
        }
      }
      return false;
    });
    expect(hasAppCss).toBe(false);
  });
}, 300_000);
