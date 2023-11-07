import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  addWebAuthnCredential,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  runInBrowser,
  switchToPopup,
} from "./util";
import { AuthenticateView, DemoAppView } from "./views";

import {
  DEVICE_NAME1,
  II_URL,
  REPLICA_URL,
  TEST_APP_CANISTER_ID,
  TEST_APP_CANONICAL_URL,
  TEST_APP_CANONICAL_URL_LEGACY,
  TEST_APP_NICE_URL,
} from "./constants";

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

    const principal1 = await canonicalDemoAppView.waitForAuthenticated();

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

    const principal2 = await niceDemoAppView.waitForAuthenticated();
    expect(principal1).toEqual(principal2);
  });
}, 300_000);

test("Should issue the same principal to dapps on legacy & official domains", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    // create a new anchor
    const registrationAuthenticator = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    await FLOWS.addRecoveryMechanismSeedPhrase(browser); // avoids being prompted later during authz
    const credentials = await getWebAuthnCredentials(
      browser,
      registrationAuthenticator
    );
    expect(credentials).toHaveLength(1);

    // Authenticate to the test dapp using the provided URL, returning the principal
    const authenticate = async (url: string): Promise<string> => {
      const canonicalDemoAppView = new DemoAppView(browser);
      await canonicalDemoAppView.open(url, II_URL);
      await canonicalDemoAppView.waitForDisplay();
      await canonicalDemoAppView.signin();

      const authzAuthenticator = await switchToPopup(browser);
      await addWebAuthnCredential(
        browser,
        authzAuthenticator,
        credentials[0],
        originToRelyingPartyId(II_URL)
      );
      let authenticateView = new AuthenticateView(browser);
      await authenticateView.waitForDisplay();
      await authenticateView.pickAnchor(userNumber);

      return canonicalDemoAppView.waitForAuthenticated();
    };

    // Compare that principals issues for ic0.app & icp0.io are the same
    const principal1 = await authenticate(TEST_APP_CANONICAL_URL);
    const principal2 = await authenticate(TEST_APP_CANONICAL_URL_LEGACY);

    expect(principal1).toEqual(principal2);
  });
}, 300_000);
