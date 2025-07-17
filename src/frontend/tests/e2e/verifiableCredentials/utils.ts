import { FLOWS } from "../flows";
import {
  addVirtualAuthenticator,
  addWebAuthnCredential,
  getWebAuthnCredentials,
  originToRelyingPartyId,
  switchToPopup,
  waitToClose,
} from "../util";
import {
  AuthenticateView,
  DemoAppView,
  IssuerAppView,
  PinAuthView,
  VcAllowView,
  VcTestAppView,
} from "../views";

import {
  II_URL,
  ISSUER_APP_URL,
  ISSUER_CANISTER_ID,
  REPLICA_URL,
} from "../constants";

import { idlFactory as vc_issuer_idl } from "$lib/generated/vc_issuer_idl";
import { KnownDapp } from "$lib/legacy/flows/dappsExplorer/dapps";
import { Actor, ActorSubclass, HttpAgent } from "@dfinity/agent";
import { _SERVICE } from "@dfinity/internet-identity-vc-api";
import { Principal } from "@dfinity/principal";
import { nonNullish } from "@dfinity/utils";

/**
 * Authenticate with the issuer app and return the principal. Do _not_ register as an employee.
 * @param browser The browser to use
 * @param issuer The issuer app URL
 * @param derivationOrigin The derivation origin to be used for the authentication process, if any.
 * @param authConfig The authentication configuration for II (i.e. how to complete the auth process on II)
 */
export const authenticateWithIssuer = async ({
  browser,
  issuer,
  derivationOrigin,
  authConfig,
}: {
  browser: WebdriverIO.Browser;
  issuer: string;
  derivationOrigin?: string;
  authConfig: AuthConfig;
}): Promise<{ principal: string }> => {
  const issuerAppView = new IssuerAppView(browser);
  await issuerAppView.open({
    issuerAppUrl: issuer,
    iiUrl: II_URL,
  });
  await issuerAppView.waitForDisplay();
  expect(await issuerAppView.isAuthenticated()).toBe(false);
  const principal = await authenticateWithIssuer_({
    browser,
    issuerAppView,
    derivationOrigin,
    authConfig,
  });

  return { principal };
};

// Open the issuer demo, authenticate and register as an employee
export const registerWithIssuer = async ({
  browser,
  issuer,
  derivationOrigin,
  authConfig,
}: {
  browser: WebdriverIO.Browser;
  issuer: string;
  derivationOrigin?: string;
  authConfig: AuthConfig;
}): Promise<{ msg: string; principal: string }> => {
  const issuerAppView = new IssuerAppView(browser);
  await issuerAppView.open({
    issuerAppUrl: issuer,
    iiUrl: II_URL,
  });
  await issuerAppView.waitForDisplay();
  expect(await issuerAppView.isAuthenticated()).toBe(false);

  const principal = await authenticateWithIssuer_({
    browser,
    issuerAppView,
    derivationOrigin,
    authConfig,
  });
  const msg = await issuerAppView.addEmployee();
  return { principal, msg };
};

const authenticateWithIssuer_ = async ({
  browser,
  issuerAppView,
  derivationOrigin,
  authConfig,
}: {
  browser: WebdriverIO.Browser;
  issuerAppView: IssuerAppView;
  derivationOrigin?: string;
  authConfig: AuthConfig;
}): Promise<string> => {
  if (nonNullish(derivationOrigin)) {
    await issuerAppView.setDerivationOrigin({ derivationOrigin });
  }
  await issuerAppView.authenticate();

  await authenticateOnII({ authConfig, browser });
  return issuerAppView.waitForAuthenticated();
};

export const authenticateOnII = async ({
  authConfig: { setupAuth, finalizeAuth, userNumber },
  browser,
}: {
  authConfig: AuthConfig;
  browser: WebdriverIO.Browser;
}): Promise<void> => {
  await setupAuth(browser);

  const authenticateView = new AuthenticateView(browser);
  await authenticateView.waitForDisplay();
  await authenticateView.pickAnchor(userNumber);

  await finalizeAuth(browser);
  await waitToClose(browser);
};

// Open the specified test app on the URL `relyingParty` and authenticate
export const authenticateToRelyingParty = async ({
  browser,
  authConfig,
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
  await vcTestApp.open(relyingParty, II_URL, issuer, ISSUER_CANISTER_ID);

  if (nonNullish(derivationOrigin)) {
    const demoView = new DemoAppView(browser);
    await demoView.setDerivationOrigin(derivationOrigin);

    await demoView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${relyingParty}"]}`,
      "certified",
    );

    await vcTestApp.setAlternativeOrigin(derivationOrigin);
  }
  await vcTestApp.startSignIn();
  await authenticateOnII({ authConfig, browser });
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
      "Expected successful VC flow, got error: " + JSON.stringify(result),
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

    return {
      userNumber,
      setupAuth: async (browser: WebdriverIO.Browser) => {
        const authenticatorId = await switchToPopup(browser);
        await addWebAuthnCredential(
          browser,
          authenticatorId,
          credentials[0],
          originToRelyingPartyId(II_URL),
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

export const setIssuerDerivationOrigin = async ({
  issuerCanisterId,
  frontendHostname,
  derivationOrigin,
}: {
  issuerCanisterId: string;
  frontendHostname: string;
  derivationOrigin: string;
}): Promise<void> => {
  const actor = await createIssuerActor(issuerCanisterId);
  await actor.set_derivation_origin(frontendHostname, derivationOrigin);
};

export const setIssuerAlternativeOrigins = async ({
  issuerCanisterId,
  alternativeOrigins,
}: {
  issuerCanisterId: string;
  alternativeOrigins: string;
}): Promise<void> => {
  const actor = await createIssuerActor(issuerCanisterId);
  await actor.set_alternative_origins(alternativeOrigins);
};

export const resetIssuerOriginsConfig = async ({
  issuerCanisterId,
}: {
  issuerCanisterId: string;
}): Promise<void> => {
  const actor = await createIssuerActor(issuerCanisterId);
  await actor.set_derivation_origin(ISSUER_APP_URL, ISSUER_APP_URL);
  await actor.set_alternative_origins('{"alternativeOrigins":[]}');
};

export const addEmployeeToIssuer = async ({
  issuerCanisterId,
  principal,
}: {
  issuerCanisterId: string;
  principal: string;
}): Promise<void> => {
  const actor = await createIssuerActor(issuerCanisterId);
  await actor.add_employee(Principal.from(principal));
};

const createIssuerActor = async (
  issuerCanisterId: string,
): Promise<ActorSubclass<_SERVICE>> => {
  const agent = await HttpAgent.create({
    host: REPLICA_URL,
    shouldFetchRootKey: true,
  });
  return Actor.createActor<_SERVICE>(vc_issuer_idl, {
    agent: agent,
    canisterId: issuerCanisterId,
  });
};
