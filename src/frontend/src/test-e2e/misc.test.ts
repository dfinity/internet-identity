import { AuthenticateView, DemoAppView, WelcomeView } from "./views";
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

import {
  II_URL,
  TEST_APP_CANISTER_ID,
  TEST_APP_NICE_URL,
  DEVICE_NAME1,
  TEST_APP_CANONICAL_URL,
  REPLICA_URL,
} from "./constants";

test("Authorize ready message should be sent immediately", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();
    await demoAppView.waitForNthMessage(1);
    expect(await demoAppView.getMessageText(1)).toContain("authorize-ready");
  });
}, 300_000);

test("Should allow valid message", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();
    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);
    await demoAppView.waitForNthMessage(1); // message 1: authorize-ready
    await demoAppView.sendValidMessage(); // message 2: authorize-client

    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(DEVICE_NAME1, browser);
    await browser
      .$("[data-role=notify-auth-success]")
      .waitForDisplayed({ timeout: 15_000 });

    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);

    await demoAppView.waitForNthMessage(3); // message 3: authorize-success
    const successMessage = await demoAppView.getMessageText(3);
    expect(successMessage).toContain("authorize-client-success");

    // Internet Identity default value (as opposed to agent-js)
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (30 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Should ignore invalid data and allow second valid message", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();
    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);
    await demoAppView.waitForNthMessage(1); // message 1: authorize-ready
    await demoAppView.sendInvalidData(); // message 2
    await demoAppView.sendValidMessage(); // message 3

    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(DEVICE_NAME1, browser);
    await browser
      .$("[data-role=notify-auth-success]")
      .waitForDisplayed({ timeout: 15_000 });

    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);

    await demoAppView.waitForNthMessage(4); // message 4: authorize-success
    const successMessage = await demoAppView.getMessageText(4);
    expect(successMessage).toContain("authorize-client-success");

    // Internet Identity default value (as opposed to agent-js)
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (30 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Should issue the same principal to nice url and canonical url", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const canonicalDemoAppView = new DemoAppView(browser);
    await canonicalDemoAppView.open(TEST_APP_CANONICAL_URL, II_URL);
    await canonicalDemoAppView.waitForDisplay();
    await canonicalDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL);
    expect(await canonicalDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await canonicalDemoAppView.signin();

    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );
    let authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    await waitToClose(browser);

    const principal1 = await canonicalDemoAppView.getPrincipal();

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID,
      `{"alternativeOrigins":["${TEST_APP_NICE_URL}"]}`,
      "certified"
    );
    await niceDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL);
    expect(await niceDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await niceDemoAppView.signin();

    const authenticatorId3 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId3,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );
    authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    await waitToClose(browser);

    const principal2 = await niceDemoAppView.getPrincipal();
    expect(principal1).toEqual(principal2);
  });
}, 300_000);
