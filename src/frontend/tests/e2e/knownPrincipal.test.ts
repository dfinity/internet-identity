import { II_URL, TEST_APP_NICE_URL } from "./constants";
import { FLOWS } from "./flows";
import {
  addWebAuthnCredential,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  runInBrowser,
  switchToPopup,
} from "./util";
import { AuthenticateView, DemoAppView } from "./views";
import { expect } from "vitest";

test("Should prompt for passkey auth immediately when supplying known auto-select principal", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { demoAppView, credentials } = await registerAndSignIn(browser);
    const exp1 = await browser.$("#expiration").getText();

    // authenticate again, but this time _with_ a known principal
    const knownPrincipal = await demoAppView.getPrincipal();
    await demoAppView.setAutoSelectionPrincipal(knownPrincipal);
    await demoAppView.signin();

    // add credential previously registered to the new tab again
    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL),
    );

    // Passkey interaction completes automatically with virtual authenticator
    await demoAppView.waitForAuthenticated();

    // By having different expiry timestamps we know that the sign-in completed
    // twice successfully
    const exp2 = await browser.$("#expiration").getText();
    expect(exp1).not.toBe(exp2);
  });
}, 300_000);

test("Should require user interaction when supplying unknown auto-select principal", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { demoAppView, credentials, userNumber } =
      await registerAndSignIn(browser);
    const exp1 = await browser.$("#expiration").getText();

    // authenticate again, but this time with an unknown principal
    // we use a canister id here, because II will never issue a canister id to users, but it is a valid principal
    await demoAppView.setAutoSelectionPrincipal("rdmx6-jaaaa-aaaaa-aaadq-cai");
    await demoAppView.signin();

    // add credential previously registered to the new tab again
    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL),
    );

    const authView = new AuthenticateView(browser);
    await authView.waitForDisplay();
    // needs explicit identity selection
    await authView.pickAnchor(userNumber);

    // Passkey interaction completes automatically with virtual authenticator
    await demoAppView.waitForAuthenticated();

    // By having different expiry timestamps we know that the sign-in completed
    // twice successfully
    const exp2 = await browser.$("#expiration").getText();
    expect(exp1).not.toBe(exp2);
  });
}, 300_000);

test("Should require user interaction when supplying not most recent auto-select principal", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { demoAppView, credentials, userNumber } =
      await registerAndSignIn(browser);
    const principal = await demoAppView.waitForAuthenticated();

    // register a second identity
    await registerAndSignIn(browser);
    const principal2 = await demoAppView.waitForAuthenticated();
    expect(principal).not.toBe(principal2);

    // authenticate again, but supply the first (older) known principal
    await demoAppView.setAutoSelectionPrincipal(principal);
    await demoAppView.signin();

    // add credential previously registered to the new tab again
    const authenticatorId2 = await switchToPopup(browser);
    await addWebAuthnCredential(
      browser,
      authenticatorId2,
      credentials[0],
      originToRelyingPartyId(II_URL),
    );

    const authView = new AuthenticateView(browser);
    await authView.waitForDisplay();
    // needs explicit identity selection
    await authView.pickAnchor(userNumber);

    // Passkey interaction completes automatically with virtual authenticator
    const principal3 = await demoAppView.waitForAuthenticated();
    // We are signed in as the first user again.
    expect(principal3).toBe(principal);
  });
}, 300_000);

/**
 * Registers a user and signs in with the demo app.
 * @param browser browser to use.
 * @return - the demo app view to proceed with the test
 *         - the identity number of the registered identity
 *         - the webauthn credential that was created when registering the identity.
 */
async function registerAndSignIn(browser: WebdriverIO.Browser) {
  const demoAppView = new DemoAppView(browser);
  await demoAppView.open(TEST_APP_NICE_URL, II_URL);
  await demoAppView.waitForDisplay();
  expect(await demoAppView.getPrincipal()).toBe("");
  await demoAppView.signin();
  const authenticatorId1 = await switchToPopup(browser);
  const userNumber = await FLOWS.registerNewIdentityAuthenticateView(browser);
  const credentials = await getWebAuthnCredentials(browser, authenticatorId1);
  expect(credentials).toHaveLength(1);
  await demoAppView.waitForAuthenticated();
  return { demoAppView, credentials, userNumber };
}
