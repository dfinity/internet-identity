import { runInBrowser } from "../util";
import { DemoAppView } from "../views";

import {
  II_URL,
  ISSUER_APP_URL,
  ISSUER_CANISTER_ID,
  ISSUER_CUSTOM_ORIGIN_NICE_URL,
  KNOWN_TEST_DAPP,
  TEST_APP_CANONICAL_URL,
  TEST_APP_NICE_URL,
} from "../constants";

import { beforeEach } from "vitest";
import {
  authenticateToRelyingParty,
  authenticateWithIssuer,
  getVCPresentation,
  getVCPresentation_,
  register,
  registerWithIssuer,
  resetIssuerOriginsConfig,
  setIssuerAlternativeOrigins,
  setIssuerDerivationOrigin,
} from "./utils";

beforeEach(async () => {
  await resetIssuerOriginsConfig({ issuerCanisterId: ISSUER_CANISTER_ID });
});

test("Can issue credential with alternative RP derivation origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const authConfig = await register["webauthn"](browser);

    // Add user as employee on the issuer
    await registerWithIssuer({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
    });

    // Authenticate to RP without alt origins
    const vcTestApp = await authenticateToRelyingParty({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      relyingParty: TEST_APP_CANONICAL_URL,
    });
    const principalRP = await vcTestApp.getPrincipal();

    // Do the VC flow without alt origins
    const { alias: alias_ } = await getVCPresentation({
      vcTestApp,
      browser,
      authConfig,
      relyingParty: TEST_APP_CANONICAL_URL,
      issuer: ISSUER_APP_URL,
    });
    const alias = JSON.parse(alias_);

    // Set up alternative origin for RP VC flow
    const demoAppView = new DemoAppView(browser);
    await demoAppView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${TEST_APP_NICE_URL}"]}`,
      "certified",
    );

    // Authenticate to RP WITH alt origins
    const vcTestAppAlt = await authenticateToRelyingParty({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      relyingParty: TEST_APP_NICE_URL,
      derivationOrigin: TEST_APP_CANONICAL_URL,
    });
    const principalRPAlt = await vcTestAppAlt.getPrincipal();
    expect(principalRPAlt).toBe(principalRP);

    // Do the VC flow WITH alt origins
    const { alias: aliasAlt_ } = await getVCPresentation({
      vcTestApp: vcTestAppAlt,
      browser,
      authConfig,
      relyingParty: TEST_APP_NICE_URL,
      issuer: ISSUER_APP_URL,
      knownDapps: [KNOWN_TEST_DAPP],
    });

    const aliasAlt = JSON.parse(aliasAlt_);
    expect(aliasAlt.sub).toBe(alias.sub);
  });
}, 300_000);

test("Cannot issue credential with bad alternative RP derivation origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const authConfig = await register["webauthn"](browser);

    // 1. Set up alternative origin for RP
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${TEST_APP_NICE_URL}"]}`,
      "certified",
    );

    // 2. Do the flow WITH alt origins RESET
    const vcTestAppAlt = await authenticateToRelyingParty({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      relyingParty: TEST_APP_NICE_URL,
      derivationOrigin: TEST_APP_CANONICAL_URL,
    });

    // 3. Reset alternative origins
    await demoAppView.resetAlternativeOrigins();

    // 4. Do the flow WITH alt origins RESET
    const result = await getVCPresentation_({
      vcTestApp: vcTestAppAlt,
      browser,
      authConfig,
      relyingParty: TEST_APP_NICE_URL,
      issuer: ISSUER_APP_URL,
      knownDapps: [KNOWN_TEST_DAPP],
    });

    expect(result.result).toBe("aborted");
    if (!("reason" in result)) {
      throw new Error(
        "Expected VC result to be aborted, got: " + JSON.stringify(result),
      );
    }
    expect(result.reason).toBe("bad_derivation_origin_rp");
  });
}, 300_000);

test("Can issue credential with alternative issuer derivation origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const authConfig = await register["webauthn"](browser);
    // Nice URL for the issuer
    const issuer = ISSUER_CUSTOM_ORIGIN_NICE_URL;
    // Derivation origin used on the nice issuer URL
    const derivationOrigin = ISSUER_APP_URL;
    const relyingParty = TEST_APP_CANONICAL_URL;

    // authenticate to the issuer without alternative origins so that we can check that the alternative origins configuration is taking effect
    // we do not add an employee, making sure the flow will later only succeed if alternative origins is working correctly.
    const { principal: principalIss } = await authenticateWithIssuer({
      browser,
      issuer,
      authConfig,
    });

    // configure issuer
    await setIssuerAlternativeOrigins({
      issuerCanisterId: ISSUER_CANISTER_ID,
      alternativeOrigins: `{"alternativeOrigins":["${issuer}"]}`,
    });
    await setIssuerDerivationOrigin({
      issuerCanisterId: ISSUER_CANISTER_ID,
      frontendHostname: issuer,
      derivationOrigin,
    });
    const { principal: principalAlt } = await registerWithIssuer({
      browser,
      issuer,
      derivationOrigin,
      authConfig,
    });
    // make sure alternative origins works as expected
    expect(principalAlt).not.toBe(principalIss);

    // Go through the VC flow
    const vcTestApp = await authenticateToRelyingParty({
      browser,
      issuer,
      authConfig,
      relyingParty,
    });
    const { alias: _alias } = await getVCPresentation({
      vcTestApp,
      browser,
      authConfig,
      relyingParty,
      issuer,
      knownDapps: [KNOWN_TEST_DAPP],
    });
  });
}, 300_000);

test("Cannot issue credential with bad alternative issuer derivation origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);
    const authConfig = await register["webauthn"](browser);
    // Nice URL for the issuer
    const issuer = ISSUER_CUSTOM_ORIGIN_NICE_URL;
    // Derivation origin used on the nice issuer URL
    const derivationOrigin = ISSUER_APP_URL;
    const relyingParty = TEST_APP_CANONICAL_URL;

    // authenticate to the issuer without alternative origins so that we can check that the alternative origins configuration is taking effect
    // we do not add an employee, making sure the flow will later only succeed if alternative origins is working correctly.
    const { principal: principalIss } = await authenticateWithIssuer({
      browser,
      issuer,
      authConfig,
    });

    // configure issuer
    await setIssuerAlternativeOrigins({
      issuerCanisterId: ISSUER_CANISTER_ID,
      alternativeOrigins: `{"alternativeOrigins":["${issuer}"]}`,
    });
    await setIssuerDerivationOrigin({
      issuerCanisterId: ISSUER_CANISTER_ID,
      frontendHostname: issuer,
      derivationOrigin,
    });
    const { principal: principalAlt } = await registerWithIssuer({
      browser,
      issuer,
      derivationOrigin,
      authConfig,
    });
    // make sure alternative origins works as expected
    expect(principalAlt).not.toBe(principalIss);

    // remove the alternative origin again
    await setIssuerAlternativeOrigins({
      issuerCanisterId: ISSUER_CANISTER_ID,
      alternativeOrigins: '{"alternativeOrigins":[]}',
    });

    // Go through the VC flow
    const vcTestApp = await authenticateToRelyingParty({
      browser,
      issuer,
      authConfig,
      relyingParty,
    });
    const result = await getVCPresentation_({
      vcTestApp,
      browser,
      authConfig,
      relyingParty,
      issuer,
      knownDapps: [KNOWN_TEST_DAPP],
    });
    expect(result.result).toBe("aborted");
    if (!("reason" in result)) {
      throw new Error(
        "Expected VC result to be aborted, got: " + JSON.stringify(result),
      );
    }
    expect(result.reason).toBe("invalid_derivation_origin_issuer");
  });
}, 300_000);
