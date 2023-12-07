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
  AuthenticateView,
  IssuerAppView,
  PinAuthView,
  VcAllowView,
  VcTestAppView,
} from "./views";

import {
  II_URL,
  ISSUER_APP_URL,
  ISSUER_APP_URL_LEGACY,
  TEST_APP_CANONICAL_URL,
  TEST_APP_CANONICAL_URL_LEGACY,
} from "./constants";

test("Can add employee on issuer app", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const issuerAppView = new IssuerAppView(browser);
    await issuerAppView.open({ issuerAppUrl: ISSUER_APP_URL, iiUrl: II_URL });
    await issuerAppView.waitForDisplay();
    expect(await issuerAppView.isAuthenticated()).toBe(false);
    await issuerAppView.authenticate();
    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(browser);
    await waitToClose(browser);
    const principal = await issuerAppView.waitForAuthenticated();
    const msg = await issuerAppView.addEmployee();
    expect(msg).toContain("Added");
    expect(msg).toContain(principal);
  });
}, 300_000);

// The different ways to register (webauthn, pin).
// The registration function returns the user number and callbacks
// used for authenticating.
const register = {
  webauthn: async (browser: WebdriverIO.Browser) => {
    const authenticatorId = await addVirtualAuthenticator(browser);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId);

    return {
      userNumber,
      setupAuth: async (browser: WebdriverIO.Browser) => {
        const authenticatorId = await switchToPopup(browser);
        await addWebAuthnCredential(
          browser,
          authenticatorId,
          credentials[0],
          originToRelyingPartyId(II_URL)
        );
      },
      finalizeAuth: () => {
        /* noop */
      },
    };
  },

  pin: async (browser: WebdriverIO.Browser) => {
    const pin = "123456";
    const userNumber = await FLOWS.registerPinWelcomeView(browser, pin);

    return {
      userNumber,
      setupAuth: async (browser: WebdriverIO.Browser) => {
        const _authenticatorId = await switchToPopup(browser);
      },
      finalizeAuth: async (browser: WebdriverIO.Browser) => {
        const pinAuthView = new PinAuthView(browser);
        await pinAuthView.waitForDisplay();
        await pinAuthView.enterPin(pin);
      },
    };
  },
};

const getDomain = (url: string) => url.split(".").slice(1).join(".");

// The different test configs (different URLs, differnet auth methods)
const testConfigs: Array<{
  relyingParty: string;
  issuer: string;
  authType: "pin" | "webauthn";
}> = [
  {
    relyingParty: TEST_APP_CANONICAL_URL_LEGACY,
    issuer: ISSUER_APP_URL,
    authType: "webauthn",
  },
  {
    relyingParty: TEST_APP_CANONICAL_URL,
    issuer: ISSUER_APP_URL_LEGACY,
    authType: "webauthn",
  },
  {
    relyingParty: TEST_APP_CANONICAL_URL,
    issuer: ISSUER_APP_URL,
    authType: "pin",
  },
];

testConfigs.forEach(({ relyingParty, issuer, authType }) => {
  const testSuffix = `RP: ${getDomain(relyingParty)}, ISS: ${getDomain(
    issuer
  )}, auth: ${authType}`;

  test(
    "Can issue credentials " + testSuffix,
    async () => {
      await runInBrowser(async (browser: WebdriverIO.Browser) => {
        await browser.url(II_URL);

        const { userNumber, setupAuth, finalizeAuth } = await register[
          authType
        ](browser);

        await FLOWS.addRecoveryMechanismSeedPhrase(browser);

        // 1. Add employee

        const issuerAppView = new IssuerAppView(browser);
        await issuerAppView.open({
          issuerAppUrl: issuer,
          iiUrl: II_URL,
        });
        await issuerAppView.waitForDisplay();
        await issuerAppView.authenticate();

        await setupAuth(browser);

        const authenticateView = new AuthenticateView(browser);
        await authenticateView.waitForDisplay();
        await authenticateView.pickAnchor(userNumber);

        await finalizeAuth(browser);
        await waitToClose(browser);

        const _principal = await issuerAppView.waitForAuthenticated();
        const _msg = await issuerAppView.addEmployee();

        // 2. Auth to RP

        const vcTestApp = new VcTestAppView(browser);
        await vcTestApp.open(relyingParty, II_URL, issuer);

        await vcTestApp.startSignIn();

        await setupAuth(browser);

        const authenticateView2 = new AuthenticateView(browser);
        await authenticateView2.waitForDisplay();
        await authenticateView2.pickAnchor(userNumber);

        await finalizeAuth(browser);
        await waitToClose(browser);

        await vcTestApp.waitForAuthenticated();
        await vcTestApp.startVcFlow();

        await setupAuth(browser);

        const vcAllow = new VcAllowView(browser);
        await vcAllow.waitForDisplay();
        const userNumber_ = await vcAllow.getUserNumber();
        expect(userNumber_).toBe(userNumber);
        await vcAllow.allow();

        await finalizeAuth(browser);
        await waitToClose(browser);

        // XXX: We don't verify the presentation (yet)
      });
    },
    300_000
  );
});
