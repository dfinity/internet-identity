import { FLOWS } from "../flows";
import {
  addVirtualAuthenticator,
  addWebAuthnCredential,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  runInBrowser,
  switchToPopup,
} from "../util";
import { AuthenticateView, DemoAppView, ErrorView } from "../views";

import { nonNullish } from "@dfinity/utils";
import {
  II_URL,
  TEST_APP_CANONICAL_URL,
  TEST_APP_CANONICAL_URL_RAW,
  TEST_APP_NICE_URL,
} from "../constants";

test("Should not issue delegation when /.well-known/ii-alternative-origins has too many entries", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      '{"alternativeOrigins":["https://a0.com", "https://a1.com", "https://a2.com", "https://a3.com", "https://a4.com", "https://a5.com", "https://a6.com", "https://a7.com", "https://a8.com", "https://a9.com", "https://a10.com"]}',
      "certified",
    );
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
      `Resource ${TEST_APP_CANONICAL_URL}/.well-known/ii-alternative-origins has too many entries: To prevent misuse at most 10 alternative origins are allowed.`,
    );
  });
}, 300_000);

test("Should not follow redirect returned by /.well-known/ii-alternative-origins", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    await FLOWS.registerNewIdentityWelcomeView(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      '{"alternativeOrigins":["https://evil.com"]}',
      "redirect",
    );
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
      `An error occurred while validating the derivationOrigin "${TEST_APP_CANONICAL_URL}": Failed to fetch`,
    );
  });
}, 300_000);

test("Should fetch /.well-known/ii-alternative-origins using the non-raw url", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_NICE_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${TEST_APP_NICE_URL}"]}`,
      "certified",
    );
    await niceDemoAppView.setDerivationOrigin(TEST_APP_CANONICAL_URL_RAW);
    expect(await niceDemoAppView.getPrincipal()).toBe("");
    await niceDemoAppView.signin();

    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL),
    );

    // Selenium has _no_ connectivity to the raw url
    // We want accessing raw urls to fail because it would be a security issue on mainnet
    await browser.execute(
      (rawURL: string) =>
        fetch(`${rawURL}/.well-known/ii-alternative-origins`).catch(
          console.error,
        ),
      TEST_APP_CANONICAL_URL_RAW,
    );

    const logs = (await browser.getLogs("browser")) as { message: string }[];
    const errorLog = logs.find(
      ({ message }) =>
        message.includes("/.well-known/ii-alternative-origins") &&
        message.includes("Failed to load resource"),
    );
    expect(nonNullish(errorLog)).toBeTruthy();

    // This works anyway --> fetched using non-raw
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    await niceDemoAppView.waitForAuthenticated();
  });
}, 300_000);

test("Should allow arbitrary URL as derivation origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authenticatorId1 = await addVirtualAuthenticator(browser);
    await browser.url(II_URL);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
    expect(credentials).toHaveLength(1);

    const niceDemoAppView = new DemoAppView(browser);
    await niceDemoAppView.open(TEST_APP_CANONICAL_URL, II_URL);
    await niceDemoAppView.waitForDisplay();
    await niceDemoAppView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${TEST_APP_CANONICAL_URL}"]}`,
      "certified",
    );
    await niceDemoAppView.setDerivationOrigin(TEST_APP_NICE_URL);
    expect(await niceDemoAppView.getPrincipal()).toBe("");
    await niceDemoAppView.signin();

    const authenticatorId3 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId3,
      credentials[0],
      originToRelyingPartyId(II_URL),
    );
    const authenticateView = new AuthenticateView(browser);
    await authenticateView.waitForDisplay();
    await authenticateView.pickAnchor(userNumber);
    await niceDemoAppView.waitForAuthenticated();
  });
}, 300_000);
