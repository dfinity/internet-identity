import { Principal } from "@dfinity/principal";
import {
  DEVICE_NAME1,
  II_URL,
  REPLICA_URL,
  TEST_APP_CANISTER_ID,
  TEST_APP_NICE_URL,
} from "./constants";
import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  runInBrowser,
  switchToPopup,
  waitToClose,
} from "./util";
import {
  AuthenticateView,
  DemoAppView,
  MainView,
  PinAuthView,
  RegisterView,
  WelcomeView,
} from "./views";

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
    await FLOWS.loginPin(userNumber, pin, browser);
    await mainView.waitForTempKeyDisplay(DEFAULT_PIN_DEVICE_NAME);
  }, APPLE_USER_AGENT);
}, 300_000);

test("Register and log in with PIN identity, retry on wrong PIN", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const pin = "123456";
    const wrongPin = "456321";

    await browser.url(II_URL);
    const userNumber = await FLOWS.registerPinWelcomeView(browser, pin);
    const mainView = new MainView(browser);
    await mainView.waitForDisplay(); // we should be logged in
    await mainView.logout();

    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);

    const pinAuthView = new PinAuthView(browser);
    await pinAuthView.waitForDisplay();
    await pinAuthView.enterPin(wrongPin);
    await pinAuthView.waitForError();
    await pinAuthView.enterPin(pin);

    // NOTE: handle recovery nag because there is no recovery phrase
    await FLOWS.skipRecoveryNag(browser);
    const mainView2 = new MainView(browser);
    await mainView2.waitForDisplay(); // we should be logged in
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

test("Log into client application using PIN registration flow", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const pin = "123456";

    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe(
      Principal.anonymous().toText()
    );
    await demoAppView.signin();
    await switchToPopup(browser);
    await FLOWS.registerPinNewIdentityAuthenticateView(pin, browser);
    await waitToClose(browser);
    await demoAppView.waitForDisplay();
    const principal = await demoAppView.getPrincipal();
    expect(principal).not.toBe(Principal.anonymous().toText());

    expect(await demoAppView.whoami(REPLICA_URL, TEST_APP_CANISTER_ID)).toBe(
      principal
    );

    // default value
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (8 * 60 * 60_000_000_000)).toBeCloseTo(1);
  }, APPLE_USER_AGENT);
}, 300_000);

test("Register with PIN then log into client application", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const pin = "123456";

    await browser.url(II_URL);
    const userNumber = await FLOWS.registerPinWelcomeView(browser, pin);

    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe(
      Principal.anonymous().toText()
    );
    await demoAppView.signin();

    await switchToPopup(browser);

    await FLOWS.loginPin(userNumber, pin, browser);
    await waitToClose(browser);

    await demoAppView.waitForDisplay();
    const principal = await demoAppView.getPrincipal();
    expect(principal).not.toBe(Principal.anonymous().toText());
  }, APPLE_USER_AGENT);
}, 300_000);
