import { APPLE_USER_AGENT, II_URL, TEST_APP_NICE_URL } from "./constants";
import { FLOWS } from "./flows";
import { runInBrowser, switchToPopup } from "./util";
import { AuthenticateView, DemoAppView, RegisterView } from "./views";

test("Cannot register with PIN if dapp disallows PIN", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.setAllowPin(false);
    expect(await demoAppView.getPrincipal()).toBe("");
    await demoAppView.signin();
    await switchToPopup(browser);
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.register();
    const registerView = new RegisterView(browser);
    await registerView.waitForDisplay();
    await registerView.assertPinRegistrationNotShown();
  }, APPLE_USER_AGENT);
}, 300_000);

test("Cannot auth with PIN if dapp disallows PIN", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const pin = "123456";

    await browser.url(II_URL);
    const userNumber = await FLOWS.registerPinWelcomeView(browser, pin);

    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.setAllowPin(false);
    expect(await demoAppView.getPrincipal()).toBe("");
    await demoAppView.signin();

    await switchToPopup(browser);

    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    await browser.$('[data-error-code="pinNotAllowed"]').waitForDisplayed();
  }, APPLE_USER_AGENT);
}, 300_000);
