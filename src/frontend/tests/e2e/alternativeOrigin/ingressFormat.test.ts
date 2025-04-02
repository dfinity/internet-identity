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
  II_URL,
  TEST_APP_CANONICAL_URL,
  TEST_APP_NICE_URL,
} from "../constants";

test("Should not issue delegation when derivationOrigin is missing from /.well-known/ii-alternative-origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const _userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.resetAlternativeOrigins();
    await niceDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL);
    expect(await niceDemoAppView.getPrincipal()).toBe("");
    await niceDemoAppView.signin();

    const authenticatorId3 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId3,
      credentials[0],
      originToRelyingPartyId(II_URL),
    );
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      `"${TEST_APP_CANONICAL_URL}" is not a valid derivation origin for "${TEST_APP_NICE_URL}"`,
    );
    expect(await errorView.getErrorDetail()).toEqual(
      `"${TEST_APP_NICE_URL}" is not listed in the list of allowed alternative origins. Allowed alternative origins:`,
    );
  });
}, 300_000);
