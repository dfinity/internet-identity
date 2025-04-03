import { runInBrowser } from "../util";

import {
  APPLE_USER_AGENT,
  ENABLE_PIN_QUERY_PARAM_KEY,
  II_URL,
  ISSUER_APP_URL,
  ISSUER_APP_URL_LEGACY,
  ISSUER_CANISTER_ID,
  KNOWN_TEST_DAPP,
  TEST_APP_CANONICAL_URL,
  TEST_APP_CANONICAL_URL_LEGACY,
  TEST_APP_NICE_URL,
} from "../constants";

import { DemoAppView } from "../views";
import { beforeEach } from "vitest";
import {
  addEmployeeToIssuer,
  authenticateOnII,
  authenticateToRelyingParty,
  getVCPresentation,
  register,
  registerWithIssuer,
  resetIssuerOriginsConfig,
  setIssuerDerivationOrigin,
} from "./utils";

beforeEach(async () => {
  await resetIssuerOriginsConfig({ issuerCanisterId: ISSUER_CANISTER_ID });
});

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

const getDomain = (url: string) => url.split(".").slice(1).join(".");

// The different test configs (different URLs, different auth methods)
const testConfigs: Array<{
  relyingParty: string;
  issuer: string;
  authType: "pin" | "webauthn";
  iiUrl: string;
}> = [
  {
    relyingParty: TEST_APP_CANONICAL_URL_LEGACY,
    issuer: ISSUER_APP_URL,
    authType: "webauthn",
    iiUrl: II_URL,
  },
  {
    relyingParty: TEST_APP_CANONICAL_URL,
    issuer: ISSUER_APP_URL_LEGACY,
    authType: "webauthn",
    iiUrl: II_URL,
  },
  {
    relyingParty: TEST_APP_CANONICAL_URL,
    issuer: ISSUER_APP_URL,
    authType: "pin",
    iiUrl: `${II_URL}?${ENABLE_PIN_QUERY_PARAM_KEY}`,
  },
];

testConfigs.forEach(({ relyingParty, issuer, authType, iiUrl }) => {
  const testSuffix = `RP: ${getDomain(relyingParty)}, ISS: ${getDomain(
    issuer,
  )}, auth: ${authType}`;

  test(
    "Can issue credentials " + testSuffix,
    async () => {
      await runInBrowser(
        async (browser: WebdriverIO.Browser) => {
          await setIssuerDerivationOrigin({
            issuerCanisterId: ISSUER_CANISTER_ID,
            derivationOrigin: issuer,
            frontendHostname: issuer,
          });

          await browser.url(iiUrl);
          const authConfig = await register[authType](browser);

          // Add employee
          await registerWithIssuer({
            browser,
            issuer: ISSUER_APP_URL,
            authConfig,
          });

          const vcTestApp = await authenticateToRelyingParty({
            browser,
            issuer,
            authConfig,
            relyingParty,
          });

          const { alias } = await getVCPresentation({
            vcTestApp,
            browser,
            authConfig,
            relyingParty,
            issuer,
            knownDapps: [KNOWN_TEST_DAPP],
          });

          // Perform a basic check on the alias
          const principalRP = await vcTestApp.getPrincipal();
          const aliasObj = JSON.parse(alias);
          expect(aliasObj.sub).toBe(`did:icp:${principalRP}`);
        },
        authType === "pin" ? APPLE_USER_AGENT : undefined,
      );
    },
    300_000,
  );
});

test("Can issue credential with issuer front-end being hosted on a different canister", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const authConfig = await register["webauthn"](browser);
    const relyingParty = TEST_APP_CANONICAL_URL;
    // We pretend the issuer front-end is hosted on TEST_APP_NICE_URL
    // while the relying party is TEST_APP_CANONICAL_URL.
    // This is a setup where the issuer is split into two canisters, one hosting the front-end
    // and one implementing the issuer canister API.
    // This test demonstrates that this setup is possible _without_ configuring alternative origins,
    // but simply configuring the derivation origin on the issuer canister and having the relying party specify
    // the issuer canister id.
    const issuer = TEST_APP_NICE_URL;
    await setIssuerDerivationOrigin({
      issuerCanisterId: ISSUER_CANISTER_ID,
      derivationOrigin: issuer,
      frontendHostname: issuer,
    });

    const issuerFrontEnd = new DemoAppView(browser);
    await issuerFrontEnd.open(issuer, II_URL);
    await issuerFrontEnd.waitForDisplay();
    await issuerFrontEnd.signin();
    await authenticateOnII({ authConfig, browser });
    const issuerPrincipal = await issuerFrontEnd.getPrincipal();
    await addEmployeeToIssuer({
      issuerCanisterId: ISSUER_CANISTER_ID,
      principal: issuerPrincipal,
    });

    // Go through the VC flow pretending the relying party URL to be the issuer front-end
    const vcTestApp = await authenticateToRelyingParty({
      browser,
      issuer,
      authConfig,
      relyingParty,
    });
    await getVCPresentation({
      vcTestApp,
      browser,
      authConfig,
      relyingParty,
      issuer,
      knownDapps: [KNOWN_TEST_DAPP],
    });
  });
}, 300_000);
