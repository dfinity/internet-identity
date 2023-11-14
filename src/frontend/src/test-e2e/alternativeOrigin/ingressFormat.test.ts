import { FLOWS } from "../flows";
import {
  addVirtualAuthenticator,
  addWebAuthnCredential,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  runInBrowser,
  switchToPopup,
} from "../util";
import { DemoAppView, ErrorView } from "../views";

import {
  DEVICE_NAME1,
  II_URL,
  REPLICA_URL,
  TEST_APP_CANISTER_ID,
  TEST_APP_CANONICAL_URL,
  TEST_APP_NICE_URL,
} from "../constants";

test("Should not issue delegation when derivationOrigin is missing from /.well-known/ii-alternative-origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(
      DEVICE_NAME1,
      browser
    );
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.resetAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID
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
      '"https://nice-name.com" is not listed in the list of allowed alternative origins. Allowed alternative origins:'
    );
  });
}, 300_000);

test("Should not issue delegation when derivationOrigin is malformed", async () => {
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
    await niceDemoAppView.resetAlternativeOrigins(
      REPLICA_URL,
      TEST_APP_CANISTER_ID
    );
    await niceDemoAppView.setDerivationOrigin(
      "https://some-random-disallowed-url.com"
    );
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
      '"https://some-random-disallowed-url.com" is not a valid derivation origin for "https://nice-name.com"'
    );
    expect(await errorView.getErrorDetail()).toEqual(
      "derivationOrigin does not match regex /^https:\\/\\/([\\w-]+)(?:\\.raw)?\\.(?:ic0\\.app|icp0\\.io)$/"
    );
  });
}, 300_000);
