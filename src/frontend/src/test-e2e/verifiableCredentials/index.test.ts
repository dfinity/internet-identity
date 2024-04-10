import { runInBrowser } from "$src/test-e2e/util";

import {
  APPLE_USER_AGENT,
  II_URL,
  ISSUER_APP_URL,
  ISSUER_APP_URL_LEGACY,
  KNOWN_TEST_DAPP,
  TEST_APP_CANONICAL_URL,
  TEST_APP_CANONICAL_URL_LEGACY,
} from "$src/test-e2e/constants";
import { DemoAppView } from "$src/test-e2e/views";

import {
  authenticateToRelyingParty,
  getVCPresentation,
  register,
  registerWithIssuer,
} from "./utils";

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

          // Auth to RP

          let vcTestApp = await authenticateToRelyingParty({
            browser,
            issuer,
            authConfig,
            relyingParty,
          });
          const principalRP = await vcTestApp.getPrincipal();

          // Add employee

          const { msg: _msg, principal: _principal } = await registerWithIssuer(
            {
              browser,
              issuer,
              authConfig,
              principal: principalRP /* issuer uses test app as origin */,
            }
          );

          // Get VC presentation

          vcTestApp = await authenticateToRelyingParty({
            browser,
            issuer,
            authConfig,
            relyingParty,
          });

          const demoAppView = new DemoAppView(browser);
          await demoAppView.updateAlternativeOrigins(
            `{"alternativeOrigins":["${issuer}"]}`,
            "certified"
          );
          const { alias } = await getVCPresentation({
            vcTestApp,
            browser,
            authConfig,
            relyingParty,
            issuer,
            knownDapps: [KNOWN_TEST_DAPP],
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
