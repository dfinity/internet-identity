import { runInBrowser } from "$src/test-e2e/util";
import { DemoAppView } from "$src/test-e2e/views";

import {
  II_URL,
  ISSUER_APP_URL,
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

    // 1. Add employee

    const { msg: _msg, principal: _principal } = await registerWithIssuer({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
    });

    // 2. Do the flow without alt origins

    const vcTestApp = await authenticateToRelyingParty({
      browser,
      issuer: ISSUER_APP_URL,
      authConfig,
      relyingParty: TEST_APP_CANONICAL_URL,
    });
    const principalRP = await vcTestApp.getPrincipal();
    const { alias: alias_ } = await getVCPresentation({
      vcTestApp,
      browser,
      authConfig,
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

    const { alias: aliasAlt_ } = await getVCPresentation({
      vcTestApp: vcTestAppAlt,
      browser,
      authConfig,
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
    });

    expect(result.result).toBe("aborted");
    if (!("reason" in result)) {
      throw new Error("brwa");
    }
    expect(result.reason).toBe("bad_derivation_origin_rp");
  });
}, 300_000);
