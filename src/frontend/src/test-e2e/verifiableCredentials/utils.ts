import { FLOWS } from "$src/test-e2e/flows";
import {
  addVirtualAuthenticator,
  addWebAuthnCredential,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  switchToPopup,
  waitToClose,
} from "$src/test-e2e/util";
import {
  AuthenticateView,
  DemoAppView,
  IssuerAppView,
  PinAuthView,
  VcAllowView,
  VcTestAppView,
} from "$src/test-e2e/views";

import { II_URL } from "$src/test-e2e/constants";

import { KnownDapp } from "$src/flows/dappsExplorer/dapps";
import { nonNullish } from "@dfinity/utils";

// Open the issuer demo, authenticate and register as an employee
export const registerWithIssuer = async ({
  browser,
  issuer,
  principal: principal_,
  authConfig: { setupAuth, finalizeAuth, userNumber },
}: {
  browser: WebdriverIO.Browser;
  issuer: string;
  principal?: string;
  authConfig: AuthConfig;
}): Promise<{ msg: string; principal: string }> => {
  const issuerAppView = new IssuerAppView(browser);
  await issuerAppView.open({
    issuerAppUrl: issuer,
    iiUrl: II_URL,
  });
  await issuerAppView.waitForDisplay();

  if (nonNullish(principal_)) {
    const principal = principal_;
    await issuerAppView.setPrincipal({ principal });
    const msg = await issuerAppView.addEmployee();
    return { principal, msg };
  }

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
export const authenticateToRelyingParty = async ({
  browser,
  authConfig: { setupAuth, finalizeAuth, userNumber },
  issuer,
  relyingParty,
  derivationOrigin,
}: {
  browser: WebdriverIO.Browser;
  relyingParty: string;
  issuer: string;
  authConfig: AuthConfig;
  derivationOrigin?: string;
}): Promise<VcTestAppView> => {
  const vcTestApp = new VcTestAppView(browser);
  await vcTestApp.open(relyingParty, II_URL, issuer);

  if (nonNullish(derivationOrigin)) {
    const demoView = new DemoAppView(browser);
    await demoView.setDerivationOrigin(derivationOrigin);

    await demoView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${relyingParty}"]}`,
      "certified"
    );

    await vcTestApp.setAlternativeOrigin(derivationOrigin);
  }
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

export const getVCPresentation = async (args: {
  vcTestApp: VcTestAppView;
  browser: WebdriverIO.Browser;
  authConfig: AuthConfig;
  relyingParty: string;
  issuer: string;
  knownDapps?: KnownDapp[];
}): Promise<{ alias: string; credential: string }> => {
  const result = await getVCPresentation_(args);
  if (result.result === "aborted") {
    throw new Error(
      "Expected successful VC flow, got error: " + JSON.stringify(result)
    );
  }

  return result;
};

// Go through the VC flow and get the presentation
export const getVCPresentation_ = async ({
  vcTestApp,
  browser,
  authConfig: { setupAuth, finalizeAuth },
  relyingParty,
  issuer,
  knownDapps = [],
}: {
  vcTestApp: VcTestAppView;
  browser: WebdriverIO.Browser;
  authConfig: AuthConfig;
  relyingParty: string;
  issuer: string;
  knownDapps?: KnownDapp[];
}): Promise<
  | { result: "ok"; alias: string; credential: string }
  | { result: "aborted"; reason: string }
> => {
  await vcTestApp.startVcFlow();

  await setupAuth(browser);

  const vcAllow = new VcAllowView(browser);
  const ty = await vcAllow.waitForDisplay();
  if (ty === "aborted") {
    const reason = await vcAllow.getAbortReason();
    return { result: "aborted", reason };
  }

  // II will show the issuer and relying party name if they are known dapps.
  const issuerName: string =
    knownDapps.find((dapp) => dapp.hasOrigin(issuer))?.name ?? issuer;
  const rpName: string =
    knownDapps.find((dapp) => dapp.hasOrigin(relyingParty))?.name ??
    relyingParty;

  expect(await vcAllow.getIssuer()).toBe(issuerName);
  expect(await vcAllow.getRelyingParty()).toBe(rpName);
  expect(await vcAllow.hasUserNumberInput()).toBe(false);

  await vcAllow.allow();

  await finalizeAuth(browser);
  await waitToClose(browser);

  const alias = await vcTestApp.getPresentationAlias();
  const credential = await vcTestApp.getPresentationCredential();

  return { result: "ok", alias, credential };
};

// The different ways to register (webauthn, pin).
// The registration function returns an 'AuthConfig' that
// can be used for authenticating.
type AuthConfig = {
  userNumber: string;
  setupAuth: (browser: WebdriverIO.Browser) => Promise<void>;
  finalizeAuth: (browser: WebdriverIO.Browser) => Promise<void>;
};

export const register: Record<
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
