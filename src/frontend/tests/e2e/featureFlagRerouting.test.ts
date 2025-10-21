import { runInBrowser } from "./util";
import { II_URL } from "./constants";
import { NewAuthorizeView, AuthenticateView } from "./views";

const checkIfHasTailwind = (browser: WebdriverIO.Browser) => {
  return browser.execute(async () => {
    for (const style of Array.from(document.styleSheets)) {
      const rules = await style.cssRules;
      for (const rule of Array.from(rules)) {
        if (rule.cssText.includes("@layer base")) {
          // This should be tailwind only but can be made more specific
          return true;
        }
      }
    }
    return false;
  });
};

test("Should redirect to new-styling authenticate with feature flag and load app.css", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Visit the root with feature flag
    await browser.url(
      `${II_URL}/?feature_flag_discoverable_passkey_flow=true#authorize`,
    );

    // Check that we're redirected to new-authenticate page
    const newAuthorizeView = new NewAuthorizeView(browser);

    // Check that the user was redirected to the unsupported page
    // because no window connection was established
    await newAuthorizeView.waitForUnsupported();

    // Verify URL shows only "/authorize/unsupported"
    expect(await browser.getUrl()).toBe(`${II_URL}/unsupported`);

    // Check that app.css is loaded by verifying it's in the document
    const hasTailwind = await checkIfHasTailwind(browser);
    expect(hasTailwind).toBe(true);
  });
}, 300_000);

test("Should show regular view without feature flag and not load app.css", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // Visit the root without feature flag
    await browser.url(`${II_URL}`);

    // Check that we're on the regular page (not new-authenticate)
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();

    const hasTailwind = await checkIfHasTailwind(browser);
    expect(hasTailwind).toBe(false);
  });
}, 300_000);
