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
