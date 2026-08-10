import { expect, type Page } from "@playwright/test";
import { test } from "../../fixtures";
import { authorizeWithUrl, II_URL, TEST_APP_URL } from "../../utils";

// Signs in to the test app for real, leaving Internet Identity holding a
// delegation for it. `prompt=login` rather than no param, because sending
// `prompt` at all is what opts an app into having its delegation kept. Driven
// through `authorizeWithUrl` because a priming round needs its own query param
// and the `authorizePage` fixture performs exactly one round, the one under
// test.
const primeDelegation = (
  page: Page,
  signIn: (authPage: Page) => Promise<void>,
): Promise<string> =>
  authorizeWithUrl(
    page,
    TEST_APP_URL,
    `${II_URL}/authorize?prompt=login`,
    async (authPage) => {
      await signIn(authPage);
      await authPage
        .getByRole("button", { name: "Continue", exact: true })
        .click();
    },
    true,
  );

test.describe("Re-issue a delegation with ?prompt=none", () => {
  // The principal the priming sign-in produced. A silent re-issue has to
  // reproduce it exactly: the delegation is derived from the same account seed
  // either way, so a different principal would mean the wrong account.
  let expectedPrincipal: string;

  // Runs before the `authorizePage` fixture performs the round under test. Only
  // one identity exists here (the default), which is what makes a silent
  // re-issue unambiguous without a hint.
  test.beforeEach(async ({ page, identities, signInWithIdentity }) => {
    expectedPrincipal = await primeDelegation(page, (authPage) =>
      signInWithIdentity(authPage, identities[0].identityNumber),
    );
  });

  // Read here rather than in the test body: `authorizedPrincipal` resolves only
  // once the flow has finished, and the flow is held open for the duration of
  // the body by the `authorizePage` fixture.
  test.afterEach(({ authorizedPrincipal }) => {
    expect(authorizedPrincipal?.toText()).toBe(expectedPrincipal);
  });

  test.describe("over the window transport", () => {
    test.use({
      authorizeConfig: { protocol: "icrc25", prompt: "none" },
    });

    test("re-issues without interaction", ({ authorizePage }) => {
      // Deliberately empty: under `prompt=none` there is no passkey, no virtual
      // authenticator and no Continue button, because Internet Identity answers
      // without rendering anything. Reaching the afterEach with the same
      // principal is the assertion.
      expect(authorizePage).toBeDefined();
    });
  });

  test.describe("over the redirect transport", () => {
    test.use({
      authorizeConfig: {
        protocol: "icrc25",
        prompt: "none",
        transport: "redirect",
      },
    });

    test("re-issues without interaction", ({ authorizePage }) => {
      // The delegation is stored against the effective origin, not the transport
      // that fetched it, so a sign-in over the window flow is re-issuable over
      // the redirect flow.
      expect(authorizePage).toBeDefined();
    });
  });
});

// Two identities have signed in to this app, so Internet Identity holds a
// delegation for each and a silent re-issue has no unambiguous answer. `?hint=`
// is the only thing that resolves it, and the principal it names is only known
// once the priming rounds have run — hence a fixture the config depends on,
// rather than a static `test.use` value.
const hintTest = test.extend<{ primed: { first: string; second: string } }>({
  primed: async ({ page, identities, signInWithIdentity }, use) => {
    const first = await primeDelegation(page, (authPage) =>
      signInWithIdentity(authPage, identities[0].identityNumber),
    );
    const second = await primeDelegation(page, (authPage) =>
      signInWithIdentity(authPage, identities[1].identityNumber),
    );
    expect(first).not.toBe(second);
    await use({ first, second });
  },
  authorizeConfig: async ({ primed }, use) => {
    await use({ protocol: "icrc25", prompt: "none", hint: primed.first });
  },
});

hintTest.describe("Re-issue a delegation with ?hint=", () => {
  hintTest.use({
    identityConfig: {
      createIdentities: [{ name: "Test 1" }, { name: "Test 2" }],
    },
  });

  // The hint names the first identity, so that is the delegation that must come
  // back — not the second, which signed in most recently and would be the one a
  // "latest wins" implementation picked.
  hintTest.afterEach(({ authorizedPrincipal, primed }) => {
    expect(authorizedPrincipal?.toText()).toBe(primed.first);
    expect(authorizedPrincipal?.toText()).not.toBe(primed.second);
  });

  hintTest(
    "re-issues the hinted identity rather than asking which to use",
    ({ authorizePage }) => {
      // Without the hint this round would be denied as ambiguous: two identities
      // on the device, two stored delegations for this origin.
      expect(authorizePage).toBeDefined();
    },
  );
});
