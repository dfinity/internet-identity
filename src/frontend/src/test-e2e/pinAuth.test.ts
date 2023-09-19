import { II_URL } from "./constants";
import { FLOWS } from "./flows";
import { runInBrowser } from "./util";
import { MainView, RegisterView, WelcomeView } from "./views";

// The PIN auth feature is only enabled for Apple specific user agents
const APPLE_USER_AGENT =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36";

// Sample user agent for Edge on Windows
const EDGE_USER_AGENT =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36 Edg/116.0.1938.81";

test("PIN registration not enabled on non-Apple device", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    const registerView = new RegisterView(browser);
    await registerView.waitForDisplay();
    await registerView.assertPinRegistrationNotShown();
  }, EDGE_USER_AGENT);
}, 300_000);

test("Register new PIN identity", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const pin = "123456";

    await browser.url(II_URL);
    const welcomeView = new WelcomeView(browser);
    await welcomeView.waitForDisplay();
    await welcomeView.register();
    await FLOWS.registerPin(browser, pin);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay(); // we should be logged in
  }, APPLE_USER_AGENT);
}, 300_000);
