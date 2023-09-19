import { DEVICE_NAME1, II_URL } from "./constants";
import { FLOWS } from "./flows";
import { addVirtualAuthenticator, runInBrowser } from "./util";
import { MainView, RegisterView, WelcomeView } from "./views";

// The PIN auth feature is only enabled for Apple specific user agents
const APPLE_USER_AGENT =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36";

// Sample user agent for Edge on Windows
const EDGE_USER_AGENT =
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36 Edg/116.0.1938.81";

const DEFAULT_PIN_DEVICE_NAME = "Chrome on Mac OS";

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

test("Register and Log in with PIN identity", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const pin = "123456";

    await browser.url(II_URL);
    const userNumber = await FLOWS.registerPinWelcomeView(browser, pin);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay(); // we should be logged in
    await mainView.waitForTempKeyDisplay(DEFAULT_PIN_DEVICE_NAME);
    await mainView.logout();
    await FLOWS.loginPin(userNumber, pin, DEFAULT_PIN_DEVICE_NAME, browser);
  }, APPLE_USER_AGENT);
}, 300_000);

test("Should not prompt for PIN after deleting temp key", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const pin = "123456";
    await addVirtualAuthenticator(browser);

    await browser.url(II_URL);
    const userNumber = await FLOWS.registerPinWelcomeView(browser, pin);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay(); // we should be logged in
    await mainView.waitForTempKeyDisplay(DEFAULT_PIN_DEVICE_NAME);
    await FLOWS.addFidoDevice(browser);

    await mainView.waitForDisplay();
    await mainView.remove(DEFAULT_PIN_DEVICE_NAME);
    await browser.waitUntil(() => browser.isAlertOpen());
    // this is equivalent to logout as we are deleting the device that was used for authentication
    await browser.acceptAlert();

    // login now happens using the WebAuthn flow
    await FLOWS.login(userNumber, DEVICE_NAME1, browser);
  }, APPLE_USER_AGENT);
}, 300_000);
