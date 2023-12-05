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

const getDomain = (url: string) => url.split(".").slice(1).join(".");

[TEST_APP_CANONICAL_URL, TEST_APP_CANONICAL_URL_LEGACY].forEach(
  (relyingParty) =>
    [ISSUER_APP_URL, ISSUER_APP_URL_LEGACY].forEach((issuer) => {
      const testSuffix = `RP: ${getDomain(relyingParty)}, ISS: ${getDomain(
        issuer
      )}`;

      test(
        "Can issue credentials " + testSuffix,
        async () => {
          await runInBrowser(async (browser: WebdriverIO.Browser) => {
            const authenticatorId_register = await addVirtualAuthenticator(
              browser
            );
            await browser.url(II_URL);
            const userNumber = await FLOWS.registerNewIdentityWelcomeView(
              browser
            );
            await FLOWS.addRecoveryMechanismSeedPhrase(browser);
            const credentials = await getWebAuthnCredentials(
              browser,
              authenticatorId_register
            );
            expect(credentials).toHaveLength(1);

            // 1. Add employee

            const issuerAppView = new IssuerAppView(browser);
            await issuerAppView.open({
              issuerAppUrl: issuer,
              iiUrl: II_URL,
            });
            await issuerAppView.waitForDisplay();
            await issuerAppView.authenticate();

            const authenticatorId_addEmployee = await switchToPopup(browser);
            await addWebAuthnCredential(
              browser,
              authenticatorId_addEmployee,
              credentials[0],
              originToRelyingPartyId(II_URL)
            );

            const authenticateView = new AuthenticateView(browser);
            await authenticateView.waitForDisplay();
            await authenticateView.pickAnchor(userNumber);
            await waitToClose(browser);

            const _principal = await issuerAppView.waitForAuthenticated();
            const _msg = await issuerAppView.addEmployee();

            // 2. Auth to RP

            const vcTestApp = new VcTestAppView(browser);
            await vcTestApp.open(relyingParty, II_URL, issuer);

            await vcTestApp.startSignIn();

            const authenticatorId2 = await switchToPopup(browser);
            await addWebAuthnCredential(
              browser,
              authenticatorId2,
              credentials[0],
              originToRelyingPartyId(II_URL)
            );

            const authenticateView2 = new AuthenticateView(browser);
            await authenticateView2.waitForDisplay();
            await authenticateView2.pickAnchor(userNumber);

            await vcTestApp.waitForAuthenticated();
            await vcTestApp.startVcFlow();
            const authenticatorId3 = await switchToPopup(browser);
            await addWebAuthnCredential(
              browser,
              authenticatorId3,
              credentials[0],
              originToRelyingPartyId(II_URL)
            );

            const vcAllow = new VcAllowView(browser);
            await vcAllow.waitForDisplay();
            await vcAllow.typeUserNumber(userNumber);
            await vcAllow.allow();
            await waitToClose(browser);

            // XXX: We don't verify the presentation (yet)
          });
        },
        300_000
      );
    })
);
