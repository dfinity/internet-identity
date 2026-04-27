import { expect } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import {
  ALTERNATE_OPENID_PORT,
  DEFAULT_OPENID_PORT,
} from "../../fixtures/openid";
import { SSO_DISCOVERY_DOMAIN, SSO_OPENID_PORT } from "../../fixtures/sso";
import { fromBase64, II_URL } from "../../utils";

// ---------------------------------------------------------------------------
// ICRC-3 decoding — same shape as `openid.spec.ts`. Duplicated locally rather
// than shared so each spec file stays self-contained.
// ---------------------------------------------------------------------------

type Icrc3Value =
  | { Nat: bigint }
  | { Int: bigint }
  | { Blob: number[] }
  | { Text: string }
  | { Array: Icrc3Value[] }
  | { Map: [string, Icrc3Value][] };

const Icrc3Value = IDL.Rec();
Icrc3Value.fill(
  IDL.Variant({
    Nat: IDL.Nat,
    Int: IDL.Int,
    Blob: IDL.Vec(IDL.Nat8),
    Text: IDL.Text,
    Array: IDL.Vec(Icrc3Value),
    Map: IDL.Vec(IDL.Tuple(IDL.Text, Icrc3Value)),
  }),
);

function decodeIcrc3TextEntries(base64Data: string): Record<string, string> {
  const dataBytes = fromBase64(base64Data);
  const { Map: map } = IDL.decode([Icrc3Value], dataBytes)[0] as {
    Map: [string, Icrc3Value][];
  };
  return Object.fromEntries(
    map
      .filter(
        (entry): entry is [string, { Text: string }] => "Text" in entry[1],
      )
      .map(([k, { Text: text }]) => [k, text]),
  );
}

// Cover the ICRC-3 attribute consent screen — the path taken when a
// dapp's requested keys aren't all in the 1-click auto-approve
// allowlist (so the fast-path `handleIcrc3OneClickOpenIdAttributes` /
// `handleIcrc3OneClickSsoAttributes` bails and
// `handleIcrc3ConsentAttributes` shows the consent UI). The consent UI
// itself is provider-agnostic; we use direct OpenID for setup because
// it's the simplest flow that lets us exercise every UI shape.

test.describe("Authorize — explicit consent UI", () => {
  test.describe("with unscoped email", () => {
    // A single scoped `openid:issuer:email` would be auto-approved via
    // 1-click OpenID; a bare `email` never is, so the consent screen
    // appears.
    const email = "unscoped.user@example.com";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { email } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: ["email"],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      // `omit_scope: true` → certified key is the bare attribute name.
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(entries.email).toBe(email);
    });

    test("should certify the matching scoped email unscoped", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await consent.accept();
    });
  });

  test.describe("with email and verified_email merging", () => {
    // Neither is on the 1-click OpenID allowlist (unscoped), so the consent
    // handler takes the request; the view merges them into a single "Email"
    // row whose consent still emits both certified forms.
    const email = "verified.merge@example.com";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { email, email_verified: "true" } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: ["email", "verified_email"],
      },
    });

    test.afterEach(({ authorizedIcrc3Attributes }) => {
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      // Originals list preserved on the merged option → both forms certified.
      expect(entries.email).toBe(email);
      expect(entries.verified_email).toBe(email);
    });

    test("should merge into one row and certify both forms", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await consent.waitForVisible();
      // Exactly one picker row is shown (the merged "Email" row).
      await expect(consent.row("Email:")).toBeVisible();
      await expect(consent.rows).toHaveCount(1);
      await consent.continue();
    });
  });

  test.describe("with scoped and unscoped of same attribute", () => {
    // The scoped form is auto-approved on its own via 1-click OpenID, but
    // pairing it with the unscoped form forces the consent handler (not all
    // keys are on the allowlist). The dedupe logic keys by (name, omitScope),
    // so the two requests survive as distinct rows with distinct labels.
    const email = "mixed.forms@example.com";
    const issuer = `http://localhost:${DEFAULT_OPENID_PORT}`;
    const providerName = `Test OpenID ${DEFAULT_OPENID_PORT}`;

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { email } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: [`openid:${issuer}:email`, "email"],
      },
    });

    test.afterEach(({ authorizedIcrc3Attributes }) => {
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      // Both key shapes are in the certified map.
      expect(entries[`openid:${issuer}:email`]).toBe(email);
      expect(entries.email).toBe(email);
    });

    test("should show provider-labeled scoped and generic unscoped rows", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await consent.waitForVisible();
      await expect(consent.row(`${providerName} email:`)).toBeVisible();
      await expect(consent.row("Email:")).toBeVisible();
      await consent.continue();
    });
  });

  test.describe("with multi-provider unscoped email", () => {
    // User has two OpenID credentials (default + alternate); an unscoped
    // `email` resolves to options from both, rendered as a picker. Default
    // selection is the first option (default provider).
    const defaultEmail = "user@default.example";
    const altEmail = "user@alt.example";

    test.use({
      openIdConfig: {
        createUsers: [
          { port: DEFAULT_OPENID_PORT, claims: { email: defaultEmail } },
          { port: ALTERNATE_OPENID_PORT, claims: { email: altEmail } },
        ],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: ["email"],
      },
    });

    // Link both providers to the identity before running the test itself, so
    // both emails are available when the consent handler asks the canister.
    test.beforeEach(
      async ({
        page,
        identities,
        signInWithIdentity,
        signInWithOpenId,
        openIdUsers,
      }) => {
        await page.goto(II_URL + "/manage/access");
        await signInWithIdentity(page, identities[0].identityNumber);
        for (const user of openIdUsers) {
          await page.getByRole("button", { name: "Add new" }).click();
          const popupPromise = page.context().waitForEvent("page");
          await page.getByRole("button", { name: user.issuer.name }).click();
          const popup = await popupPromise;
          const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
          await signInWithOpenId(popup, user.id);
          await closePromise;
        }
      },
    );

    test.afterEach(({ authorizedIcrc3Attributes }) => {
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      // Default selection picks the first option — the default provider's email.
      expect(entries.email).toBe(defaultEmail);
    });

    test("should certify default picker option", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await consent.waitForVisible();
      // The picker button is only rendered when the row has >1 option.
      await expect(consent.pickerButton).toBeVisible();
      await consent.continue();
    });
  });

  test.describe("with all attributes denied", () => {
    const email = "denied@example.com";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { email } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: ["email"],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      // Authentication still succeeded; the ICRC-3 response is signed but
      // carries no attribute text entries.
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(entries.email).toBeUndefined();
    });

    test("should deny all rows and continue", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await consent.waitForVisible();
      await consent.denyAll();
      await consent.continue();
    });
  });

  test.describe("with one attribute unchecked", () => {
    const name = "Selective User";
    const email = "selective@example.com";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { name, email } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: ["name", "email"],
      },
    });

    test.afterEach(({ authorizedIcrc3Attributes }) => {
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(entries.name).toBe(name);
      expect(entries.email).toBeUndefined();
    });

    test("should omit only the unchecked attribute", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await consent.waitForVisible();
      await consent.uncheckRow("Email:");
      await consent.continue();
    });
  });

  test.describe("with unknown attribute keys", () => {
    // Consent handler asks the canister for everything the anchor has, then
    // resolves the request against that set on the frontend, so unknown names
    // (`favorite_color`, `ghost`) simply don't match anything rather than
    // rejecting the whole request. The consent UI only shows rows for keys
    // that resolved to something.
    const name = "Partly Known";

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { name } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: ["name", "favorite_color", "ghost"],
      },
    });

    test.afterEach(({ authorizedIcrc3Attributes }) => {
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(entries.name).toBe(name);
      expect(entries.favorite_color).toBeUndefined();
      expect(entries.ghost).toBeUndefined();
    });

    test("should silently drop the unknown keys", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await consent.waitForVisible();
      await expect(consent.row("Name:")).toBeVisible();
      await expect(consent.rows).toHaveCount(1);
      await consent.continue();
    });
  });

  test.describe("with no available attributes", () => {
    // A request whose keys all resolve to nothing available on the anchor
    // (here: user has no email claim) auto-resolves the consent result so
    // the UI never renders a blank screen.
    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: {} }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        openid: `http://localhost:${DEFAULT_OPENID_PORT}`,
        useIcrc3Attributes: true,
        attributes: ["email"],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(entries.email).toBeUndefined();
    });

    test("should skip the consent UI", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      // Handler auto-resolves → II window closes without ever rendering the
      // consent heading. The fixture waits for the window to close after the
      // test body returns; if the consent screen were ever shown, the
      // auto-close would block and this test would time out instead of the
      // assertion below firing.
      await consent.expectHidden();
    });
  });

  test.describe("with linked OpenID credential", () => {
    // User signs in via passkey, not 1-click OpenID — so `flow.type` is
    // "regular" and the 1-click OpenID fast-path never runs. Attributes
    // are still sourced from the OpenID credential that was linked to
    // the anchor ahead of time, so the consent handler has something to
    // show. Scoped requests so the row labels render with the issuer
    // name, mirroring the SSO scope test below.
    const name = "Linked User";
    const email = "linked@example.com";
    const issuer = `http://localhost:${DEFAULT_OPENID_PORT}`;
    const providerName = `Test OpenID ${DEFAULT_OPENID_PORT}`;

    test.use({
      openIdConfig: {
        defaultPort: DEFAULT_OPENID_PORT,
        createUsers: [{ claims: { name, email } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        // No `openid:` → the authorize page does not take the 1-click route.
        useIcrc3Attributes: true,
        attributes: [`openid:${issuer}:name`, `openid:${issuer}:email`],
      },
    });

    // Link the OpenID credential to the passkey identity before the test
    // runs the dapp sign-in.
    test.beforeEach(
      async ({
        page,
        identities,
        signInWithIdentity,
        signInWithOpenId,
        openIdUsers,
      }) => {
        await page.goto(II_URL + "/manage/access");
        await signInWithIdentity(page, identities[0].identityNumber);
        for (const user of openIdUsers) {
          await page.getByRole("button", { name: "Add new" }).click();
          const popupPromise = page.context().waitForEvent("page");
          await page.getByRole("button", { name: user.issuer.name }).click();
          const popup = await popupPromise;
          const closePromise = popup.waitForEvent("close", { timeout: 15_000 });
          await signInWithOpenId(popup, user.id);
          await closePromise;
        }
      },
    );

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(entries[`openid:${issuer}:email`]).toBe(email);
      expect(entries[`openid:${issuer}:name`]).toBe(name);
    });

    test("should label rows with the issuer name", async ({
      attributeConsentView,
      authorizePage,
      identities,
      signInWithIdentity,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      // Sign the passkey identity into the dapp's authorize popup, then
      // hit the final "Continue" that triggers authorization — the consent
      // handler only wakes up once the user has authorized.
      await signInWithIdentity(
        authorizePage.page,
        identities[0].identityNumber,
      );
      await authorizePage.page
        .getByRole("button", { name: "Continue", exact: true })
        .click();
      await consent.waitForVisible();
      await expect(consent.row(`${providerName} email:`)).toBeVisible();
      await expect(consent.row(`${providerName} name:`)).toBeVisible();
      await consent.continue();
    });
  });

  test.describe("with linked SSO credential", () => {
    // SSO credentials surface via `sso:<domain>:<key>`. The consent UI
    // resolves the domain's published name through the same two-hop
    // discovery the sign-in path uses, so the rows render with the
    // friendly prefix (e.g. `Test SSO 11107 email:`) rather than the
    // bare domain. We sign up via the wizard SSO flow inline since the
    // canister has no SSO credential before this test.
    const name = "Linked User";
    const email = "linked@example.com";

    test.use({
      openIdConfig: {
        defaultPort: SSO_OPENID_PORT,
        createUsers: [{ claims: { name, email } }],
      },
      authorizeConfig: {
        protocol: "icrc25",
        useIcrc3Attributes: true,
        attributes: [
          `sso:${SSO_DISCOVERY_DOMAIN}:name`,
          `sso:${SSO_DISCOVERY_DOMAIN}:email`,
        ],
      },
    });

    test.afterEach(({ authorizedPrincipal, authorizedIcrc3Attributes }) => {
      expect(authorizedPrincipal?.isAnonymous()).toBe(false);
      expect(authorizedIcrc3Attributes).toBeDefined();
      if (authorizedIcrc3Attributes === undefined) return;
      const entries = decodeIcrc3TextEntries(authorizedIcrc3Attributes.data);
      expect(entries[`sso:${SSO_DISCOVERY_DOMAIN}:name`]).toBe(name);
      expect(entries[`sso:${SSO_DISCOVERY_DOMAIN}:email`]).toBe(email);
    });

    test("should label rows with the SSO domain name", async ({
      attributeConsentView,
      authorizePage,
      openSsoPopup,
      signInWithOpenId,
      openIdUsers,
    }) => {
      const consent = attributeConsentView(authorizePage.page);
      const ssoPage = await openSsoPopup(authorizePage.page);
      const closePromise = ssoPage.waitForEvent("close", { timeout: 15_000 });
      await signInWithOpenId(ssoPage, openIdUsers[0].id);
      await closePromise;
      await authorizePage.page
        .getByRole("button", { name: "Continue", exact: true })
        .click();
      await consent.waitForVisible();
      await expect(
        consent.row(`Test SSO ${SSO_OPENID_PORT} email:`),
      ).toBeVisible();
      await expect(
        consent.row(`Test SSO ${SSO_OPENID_PORT} name:`),
      ).toBeVisible();
      await consent.continue();
    });
  });
});
