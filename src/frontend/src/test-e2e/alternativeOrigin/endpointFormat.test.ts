import { FLOWS } from "../flows";
import {
  addVirtualAuthenticator,
  addWebAuthnCredential,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  runInBrowser,
  switchToPopup,
  waitToClose,
} from "../util";
import { AuthenticateView, DemoAppView, ErrorView } from "../views";

import {
  DEVICE_NAME1,
  II_URL,
  REPLICA_URL,
  TEST_APP_CANISTER_ID,
  TEST_APP_CANONICAL_URL,
  TEST_APP_NICE_URL,
} from "../constants";

test("Should not issue delegation when /.well-known/ii-alternative-origins has too many entries", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID,
      '{"alternativeOrigins":["https://a0.com", "https://a1.com", "https://a2.com", "https://a3.com", "https://a4.com", "https://a5.com", "https://a6.com", "https://a7.com", "https://a8.com", "https://a9.com", "https://a10.com"]}',
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
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      `"${TEST_APP_CANONICAL_URL}" is not a valid derivation origin for "https://nice-name.com"`
    );
    expect(await errorView.getErrorDetail()).toEqual(
      `Resource ${TEST_APP_CANONICAL_URL}/.well-known/ii-alternative-origins has too many entries: To prevent misuse at most 10 alternative origins are allowed.`
    );
  });
}, 300_000);

test("Should not follow redirect returned by /.well-known/ii-alternative-origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(DEVICE_NAME1, browser);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID,
      '{"alternativeOrigins":["https://evil.com"]}',
      "redirect"
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
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      `"${TEST_APP_CANONICAL_URL}" is not a valid derivation origin for "https://nice-name.com"`
    );
    expect(await errorView.getErrorDetail()).toEqual(
      `An error occurred while validating the derivationOrigin "${TEST_APP_CANONICAL_URL}": Failed to fetch`
    );
  });
}, 300_000);

test("Should fetch /.well-known/ii-alternative-origins using the non-raw url", async () => {
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

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID,
      `{"alternativeOrigins":["${TEST_APP_NICE_URL}"]}`,
      "certified"
    );
    await niceDemoAppView.setDerivationOrigin(
      `https://${TEST_APP_CANISTER_ID}.raw.icp0.io`
    );
    expect(await niceDemoAppView.getPrincipal()).toBe("2vxsx-fae");
    await niceDemoAppView.signin();

    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL)
    );

    // Selenium has _no_ connectivity to the raw url
    await browser.execute(
      `console.log(await fetch("https://${TEST_APP_CANISTER_ID}.raw.icp0.io/.well-known/ii-alternative-origins"))`
    );
    let logs = (await browser.getLogs("browser")) as { message: string }[];
    expect(logs[logs.length - 1].message).toEqual(
      `https://${TEST_APP_CANISTER_ID}.raw.icp0.io/.well-known/ii-alternative-origins - Failed to load resource: the server responded with a status of 400 (Bad Request)`
    );

    // This works anyway --> fetched using non-raw
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    await waitToClose(browser);

    expect(await niceDemoAppView.getPrincipal()).not.toBe("2vxsx-fae");
  });
}, 300_000);
