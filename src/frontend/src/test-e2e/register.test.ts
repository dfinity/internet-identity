import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  addWebAuthnCredential,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  runInBrowser,
  switchToPopup,
  waitToClose,
} from "./util";
import { AuthenticateView, DemoAppView, MainView } from "./views";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import { readFileSync } from "fs";
import { DEVICE_NAME1, II_URL, REPLICA_URL } from "./constants";
export const test_app_canister_ids = JSON.parse(
  readFileSync("./demos/test-app/.dfx/local/canister_ids.json", "utf-8")
);

const TEST_APP_CANISTER_ID = test_app_canister_ids.test_app.local;
const TEST_APP_CANONICAL_URL = `https://${TEST_APP_CANISTER_ID}.ic0.app`;
const TEST_APP_NICE_URL = "https://nice-name.com";

test("Register new identity and login with it", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    await addVirtualAuthenticator(browser);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.logout();
    await FLOWS.loginAuthenticateView(userNumber, DEVICE_NAME1, browser);
  });
}, 300_000);

test("Register new identity and login without prefilled identity number", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    await addVirtualAuthenticator(browser);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    const mainView = new MainView(browser);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    // clear local storage, so that the identity number is not prefilled
    await browser.execute("localStorage.clear()");

    // load the II page again
    await browser.url(II_URL);
    await FLOWS.loginWelcomeView(userNumber, DEVICE_NAME1, browser);
  });
}, 300_000);

test("Log into client application, after registration", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.signin();
    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(DEVICE_NAME1, browser);
    await waitToClose(browser);
    await demoAppView.waitForDisplay();
    const principal = await demoAppView.getPrincipal();
    expect(principal).not.toBe("2vxsx-fae");

    expect(await demoAppView.whoami(REPLICA_URL, TEST_APP_CANISTER_ID)).toBe(
      principal
    );

    // default value
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (8 * 60 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Register first then log into client application", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);

    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );

    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.signin();

    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );

    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    await FLOWS.skipRecoveryNag(browser);
    await waitToClose(browser);
    await demoAppView.waitForDisplay();
    const principal = await demoAppView.getPrincipal();
    expect(principal).not.toBe("2vxsx-fae");

    expect(await demoAppView.whoami(REPLICA_URL, TEST_APP_CANISTER_ID)).toBe(
      principal
    );

    // default value
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (8 * 60 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);
