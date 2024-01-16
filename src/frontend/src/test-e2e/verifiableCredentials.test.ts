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
  APPLE_USER_AGENT,
  II_URL,
  ISSUER_APP_URL,
  ISSUER_APP_URL_LEGACY,
  TEST_APP_CANONICAL_URL,
  TEST_APP_CANONICAL_URL_LEGACY,
} from "./constants";

test("Can add employee on issuer app", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const authConfig = await register["webauthn"](browser);

    const { msg, principal } = await registerWithIssuer({
      browser,
      authConfig,
      issuer: ISSUER_APP_URL,
    });

    expect(msg).toContain("Added");
    expect(msg).toContain(principal);
  });
}, 300_000);

// Open the issuer demo, authenticate and register as an employee
const registerWithIssuer = async ({
  browser,
  issuer,
  authConfig: { setupAuth, finalizeAuth, userNumber },
}: {
  browser: WebdriverIO.Browser;
  issuer: string;
  authConfig: AuthConfig;
}): Promise<{ msg: string; principal: string }> => {
  const issuerAppView = new IssuerAppView(browser);
  await issuerAppView.open({
    issuerAppUrl: issuer,
    iiUrl: II_URL,
  });
  await issuerAppView.waitForDisplay();

  expect(await issuerAppView.isAuthenticated()).toBe(false);

  await issuerAppView.authenticate();

  await setupAuth(browser);

  const authenticateView = new AuthenticateView(browser);
  await authenticateView.waitForDisplay();
  await authenticateView.pickAnchor(userNumber);

  await finalizeAuth(browser);
  await waitToClose(browser);

  const principal = await issuerAppView.waitForAuthenticated();
  const msg = await issuerAppView.addEmployee();

  return { principal, msg };
};

// Open the specified test app on the URL `relyingParty` and authenticate
const authenticateToRelyingParty = async ({
  browser,
  authConfig: { setupAuth, finalizeAuth, userNumber },
  issuer,
  relyingParty,
}: {
  browser: WebdriverIO.Browser;
  relyingParty: string;
  issuer: string;
  authConfig: AuthConfig;
}): Promise<VcTestAppView> => {
  const vcTestApp = new VcTestAppView(browser);
  await vcTestApp.open(relyingParty, II_URL, issuer);

  await vcTestApp.startSignIn();

  await setupAuth(browser);

  const authenticateView = new AuthenticateView(browser);
  await authenticateView.waitForDisplay();
  await authenticateView.pickAnchor(userNumber);

  await finalizeAuth(browser);
  await waitToClose(browser);

  await vcTestApp.waitForAuthenticated();

  return vcTestApp;
};

// Go through the VC flow and get the presentation
const getVCPresentation = async ({
  vcTestApp,
  browser,
  authConfig: { setupAuth, finalizeAuth, userNumber },
}: {
  vcTestApp: VcTestAppView;
  browser: WebdriverIO.Browser;
  authConfig: AuthConfig;
}): Promise<{ alias: string; credential: string }> => {
  await vcTestApp.startVcFlow();

  await setupAuth(browser);

  const vcAllow = new VcAllowView(browser);
  await vcAllow.waitForDisplay();
  const userNumber_ = await vcAllow.getUserNumber();
  expect(userNumber_).toBe(userNumber);
  await vcAllow.allow();

  await finalizeAuth(browser);
  await waitToClose(browser);

  const alias = await vcTestApp.getPresentationAlias();
  const credential = await vcTestApp.getPresentationCredential();

  return { alias, credential };
};

// The different ways to register (webauthn, pin).
// The registration function returns an 'AuthConfig' that
// can be used for authenticating.
type AuthConfig = {
  userNumber: string;
  setupAuth: (browser: WebdriverIO.Browser) => Promise<void>;
  finalizeAuth: (browser: WebdriverIO.Browser) => Promise<void>;
};
const register: Record<
  "webauthn" | "pin",
  (browser: WebdriverIO.Browser) => Promise<AuthConfig>
> = {
  webauthn: async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const authenticatorId = await addVirtualAuthenticator(browser);
    const userNumber = await FLOWS.registerNewIdentityWelcomeView(browser);
    const credentials = await getWebAuthnCredentials(browser, authenticatorId);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);

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
      finalizeAuth: async (_: WebdriverIO.Browser) => {
        /* noop */
      },
    };
  },

  pin: async (browser: WebdriverIO.Browser) => {
    const pin = "123456";
    const userNumber = await FLOWS.registerPinWelcomeView(browser, pin);
    await FLOWS.addRecoveryMechanismSeedPhrase(browser);

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
      await runInBrowser(
        async (browser: WebdriverIO.Browser) => {
          await browser.url(II_URL);

          const authConfig = await register[authType](browser);

          // 1. Add employee

          const { msg: _msg, principal: _principal } = await registerWithIssuer(
            {
              browser,
              issuer,
              authConfig,
            }
          );

          // 2. Auth to RP

          const vcTestApp = await authenticateToRelyingParty({
            browser,
            issuer,
            authConfig,
            relyingParty,
          });

          const principalRP = await vcTestApp.getPrincipal();

          // 3. Get VC presentation

          const { alias } = await getVCPresentation({
            vcTestApp,
            browser,
            authConfig,
          });

          // Perform a basic check on the alias
          const aliasObj = JSON.parse(alias);
          expect(aliasObj.sub).toBe(`did:icp:${principalRP}`);
        },
        authType === "pin" ? APPLE_USER_AGENT : undefined
      );
    },
    300_000
  );
});
