import { expect } from "@playwright/test";
import { IDL } from "@icp-sdk/core/candid";
import { test } from "../../fixtures";
import {
  ALTERNATE_OPENID_PORT,
  DEFAULT_OPENID_PORT,
} from "../../fixtures/openid";
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

// ---------------------------------------------------------------------------
// Tests
//
// These cover the ICRC-3 attribute consent screen — the path taken when a
// dapp's requested keys can't all be handled by implicit consent (so the
// fast-path `handleIcrc3ImplicitAttributes` bails and
// `handleIcrc3ConsentAttributes` shows the consent UI).
//
// UI interactions go through the `attributeConsentView` fixture rather than
// raw role queries, so if the consent DOM shape changes we update it once in
// the fixture instead of across every spec.
// ---------------------------------------------------------------------------

test.describe("Authorize with OpenID — explicit consent UI", () => {
  test.describe("unscoped email request", () => {
    // Single scoped attribute `openid:issuer:email` would be implicit; a bare
    // `email` never is, so the consent screen appears.
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

    test("shows consent and certifies the matching scoped email unscoped", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await attributeConsentView.accept();
    });
  });

  test.describe("email + verified_email merge into one row", () => {
    // Both are non-implicit (unscoped) so the consent handler takes the
    // request; the view merges them into a single "Email" row whose consent
    // still emits both certified forms.
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

    test("one row in the UI, both forms certified", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await attributeConsentView.waitForVisible();
      // Exactly one picker row is shown (the merged "Email" row).
      await expect(attributeConsentView.row("Email:")).toBeVisible();
      await expect(attributeConsentView.rows).toHaveCount(1);
      await attributeConsentView.continue();
    });
  });

  test.describe("scoped + unscoped same attribute → two rows", () => {
    // The scoped form is implicit on its own, but pairing it with the unscoped
    // form forces the consent handler (not all keys implicit). The dedupe
    // logic keys by (name, omitScope), so the two requests survive as
    // distinct rows with distinct labels.
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

    test("shows a provider-labeled scoped row and a generic unscoped row", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await attributeConsentView.waitForVisible();
      await expect(
        attributeConsentView.row(`${providerName} email:`),
      ).toBeVisible();
      await expect(attributeConsentView.row("Email:")).toBeVisible();
      await attributeConsentView.continue();
    });
  });

  test.describe("multi-provider unscoped email → picker", () => {
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

    test("default picker option is certified", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await attributeConsentView.waitForVisible();
      // The picker button is only rendered when the row has >1 option.
      await expect(attributeConsentView.pickerButton).toBeVisible();
      await attributeConsentView.continue();
    });
  });

  test.describe("Deny All → empty attribute set", () => {
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

    test("denies all rows and continues", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await attributeConsentView.waitForVisible();
      await attributeConsentView.denyAll();
      await attributeConsentView.continue();
    });
  });

  test.describe("unchecking one row omits only that attribute", () => {
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

    test("uncheck email then continue", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await attributeConsentView.waitForVisible();
      await attributeConsentView.uncheckRow("Email:");
      await attributeConsentView.continue();
    });
  });

  test.describe("unknown attribute keys are silently dropped", () => {
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

    test("only the known key renders a consent row", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      await attributeConsentView.waitForVisible();
      await expect(attributeConsentView.row("Name:")).toBeVisible();
      await expect(attributeConsentView.rows).toHaveCount(1);
      await attributeConsentView.continue();
    });
  });

  test.describe("no available attributes skips the consent UI", () => {
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

    test("consent screen never appears", async ({
      attributeConsentView,
      authorizePage,
      signInWithOpenId,
      openIdUsers,
    }) => {
      await signInWithOpenId(authorizePage.page, openIdUsers[0].id);
      // Handler auto-resolves → II window closes without ever rendering the
      // consent heading. The fixture waits for the window to close after the
      // test body returns; if the consent screen were ever shown, the
      // auto-close would block and this test would time out instead of the
      // assertion below firing.
      await attributeConsentView.expectHidden();
    });
  });
});
