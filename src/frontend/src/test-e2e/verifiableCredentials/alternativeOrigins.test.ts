import { runInBrowser } from "$src/test-e2e/util";
import { DemoAppView } from "$src/test-e2e/views";

import {
  II_URL,
  ISSUER_APP_URL,
  ISSUER_CUSTOM_ORIGIN_NICE_URL,
  KNOWN_TEST_DAPP,
  TEST_APP_CANONICAL_URL,
  TEST_APP_NICE_URL,
} from "$src/test-e2e/constants";

import {
  authenticateToRelyingParty,
  getVCPresentation,
  getVCPresentation_,
  register,
  registerWithIssuer,
} from "./utils";

test("Can issue credential with alternative RP derivation origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);

    const authConfig = await register["webauthn"](browser);

    // XXX: The issuer always specifies a derivation origin, which we need
    // to whitelist (we use the test app because it's the only canister that
    // has an API for setting alternative origins)
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_CANONICAL_URL, II_URL);
    await demoAppView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${ISSUER_APP_URL}"]}`,
      "certified"
    );

    // Do the flow without alt origins

    let vcTestApp = await authenticateToRelyingParty({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      relyingParty: TEST_APP_CANONICAL_URL,
    });
    const principalRP = await vcTestApp.getPrincipal();

    // Add employee and set up issuer
    const { msg: _msg, principal: _principal } = await registerWithIssuer({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      principal: principalRP,
    });

    vcTestApp = await authenticateToRelyingParty({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      relyingParty: TEST_APP_CANONICAL_URL,
    });

    const { alias: alias_ } = await getVCPresentation({
      vcTestApp,
      browser,
      authConfig,
      relyingParty: TEST_APP_CANONICAL_URL,
      issuer: ISSUER_APP_URL,
    });
    const alias = JSON.parse(alias_);

    // 3. Do the flow WITH alt origins
    const vcTestAppAlt = await authenticateToRelyingParty({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      relyingParty: TEST_APP_NICE_URL,
      derivationOrigin: TEST_APP_CANONICAL_URL,
    });
    const principalRPAlt = await vcTestAppAlt.getPrincipal();
    expect(principalRPAlt).toBe(principalRP);

    // XXX: More gymnastics for alternative origins to be set correctly during VC flow
    await demoAppView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${ISSUER_APP_URL}", "${TEST_APP_NICE_URL}"]}`,
      "certified"
    );

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

    // 1. Add employee

    const { msg: _msg, principal: _principal } = await registerWithIssuer({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
    });

    // 2. Do the flow WITH alt origins RESET

    const vcTestAppAlt = await authenticateToRelyingParty({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      relyingParty: TEST_APP_NICE_URL,
      derivationOrigin: TEST_APP_CANONICAL_URL,
    });

    await new DemoAppView(browser).resetAlternativeOrigins();

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
        "Expected VC result to be aborted, got: " + JSON.stringify(result)
      );
    }
    expect(result.reason).toBe("bad_derivation_origin_rp");
  });
}, 300_000);

test("Can issue credential with alternative issuer derivation origin", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await browser.url(II_URL);

    // This runs a VC flow where the issuer uses a nice domain and uses the test-app as its
    // derivation origin. We use the test-app as derivation origin because it is easy
    // to configure its alternativeOrigins.

    const issuer = ISSUER_CUSTOM_ORIGIN_NICE_URL;
    const relyingParty = TEST_APP_CANONICAL_URL;

    const authConfig = await register["webauthn"](browser);

    // First authenticate & grab the principal (because of alternative origins, it's the same
    // for the test app and the issuer)
    let vcTestApp = await authenticateToRelyingParty({
      browser,
      issuer,
      authConfig,
      relyingParty,
    });
    const principalRP = await vcTestApp.getPrincipal();

    // Register with the issuer, with a custom principal (i.e. not one generated by the issuer's
    // origin)
    const { msg: _msg, principal: _principal } = await registerWithIssuer({
      browser,
      issuer,
      principal: principalRP,
      authConfig,
    });

    // Go back to the test app and start the VC flow

    // XXX: we use the test app as the derivation origin because it has an
    // API for updating alternative origins
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(relyingParty, II_URL);
    await demoAppView.updateAlternativeOrigins(
      `{"alternativeOrigins":["${issuer}"]}`,
      "certified"
    );

    vcTestApp = await authenticateToRelyingParty({
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
