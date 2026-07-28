import { IDL } from "@icp-sdk/core/candid";
import { Principal } from "@icp-sdk/core/principal";
import { test as base, expect, type Page } from "@playwright/test";
import { II_URL, toBase64 } from "../utils";

export type AuthorizeConfig = {
  testAppURL: string;
  internetIdentityURL: string;
  protocol: "legacy" | "icrc25";
  /**
   * Sign-in transport the test app uses. Defaults to `"window"` (the popup /
   * postMessage flow). `"redirect"` drives the ICRC-167 single-tab redirect
   * flow: clicking "Sign In" navigates the same tab to the callback page and on
   * to II, and the response is delivered back by navigation (no popup).
   */
  transport?: "window" | "redirect";
} & (
  | { protocol: "legacy" }
  | {
      protocol: "icrc25";
      /** Triggers `?openid=<issuer>` 1-click direct OpenID. */
      openid?: string;
      /**
       * Triggers `?sso=<domain>` 1-click SSO discovery. Mutually
       * exclusive with `openid` — combining the two surfaces an error
       * page rather than picking one silently.
       */
      sso?: string;
      attributes?: string[];
      useIcrc3Attributes?: boolean;
      icrc3Nonce?: Uint8Array;
    }
);

class AuthorizePage {
  readonly #page: Page;

  constructor(page: Page) {
    this.#page = page;
  }

  get page(): Page {
    return this.#page;
  }
}

export const test = base.extend<{
  authorizeConfig: Partial<AuthorizeConfig> | undefined;
  authorizePage: AuthorizePage;
  authorizedPrincipal: Principal | undefined;
  authorizedAttributes: Record<string, string> | undefined;
  authorizedIcrc3Attributes: { data: string; signature: string } | undefined;
  /**
   * The delegation chain the test app received, parsed from its `#delegation`
   * view (pubkeys and publicKey are hex). Lets a spec assert the chain's shape
   * — e.g. the redirect flow's two-hop intermediate-key structure. Waits for
   * the redirect return before reading, like {@link authorizedPrincipal};
   * `undefined` when the page has no delegation.
   */
  authorizedDelegation:
    | {
        delegations: { delegation: { pubkey: string } }[];
        publicKey: string;
      }
    | undefined;
  /**
   * The II backend canister id, discovered by visiting the II URL in a
   * throwaway page and reading `<body data-canister-id>` (the II
   * frontend canister rewrites the served HTML to embed the backend
   * canister id there at request time). Used by
   * {@link canisterEchoedIcrc3Attributes} as the `signer` for the
   * `AttributesIdentity` round-trip.
   */
  iiBackendCanisterId: Principal;
  /**
   * Triggers the test_app's "Send attributes to canister" flow against
   * the test_app canister using `AttributesIdentity` wrapping the
   * delegation identity. Returns the canister-echoed attribute map
   * (parsed from `caller_attributes`'s response) as plain string
   * entries, or `undefined` if the prior auth flow didn't produce an
   * ICRC-3 bundle. Makes the round-trip from II → frontend →
   * AttributesIdentity → canister observable in tests so the consent
   * specs can assert end-to-end attribute delivery.
   */
  canisterEchoedIcrc3Attributes: Record<string, string> | undefined;
}>({
  authorizeConfig: undefined,
  authorizePage: async ({ page, authorizeConfig }, use) => {
    if (authorizeConfig === undefined) {
      throw new Error("authorizeConfig must be defined");
    }

    await page.goto(authorizeConfig.testAppURL ?? "https://nice-name.com");
    const testAppPage = page;

    const internetIdentityURL =
      authorizeConfig.internetIdentityURL ?? "https://id.ai";
    const protocol = authorizeConfig.protocol ?? "legacy";

    if (protocol === "icrc25") {
      const queryParams: string[] = [];
      if ("openid" in authorizeConfig && authorizeConfig.openid !== undefined) {
        queryParams.push(
          `openid=${encodeURIComponent(authorizeConfig.openid)}`,
        );
      }
      if ("sso" in authorizeConfig && authorizeConfig.sso !== undefined) {
        queryParams.push(`sso=${encodeURIComponent(authorizeConfig.sso)}`);
      }
      const querySuffix =
        queryParams.length > 0 ? `?${queryParams.join("&")}` : "";
      await testAppPage
        .getByRole("textbox", { name: "Identity Provider" })
        .fill(internetIdentityURL + "/authorize" + querySuffix);
      await testAppPage
        .getByRole("checkbox", { name: "Use ICRC-25 protocol:" })
        .setChecked(true);
      if (
        "useIcrc3Attributes" in authorizeConfig &&
        authorizeConfig.useIcrc3Attributes === true
      ) {
        await testAppPage
          .getByRole("checkbox", { name: "Use ICRC-3 attributes:" })
          .setChecked(true);
      }
      if (
        "icrc3Nonce" in authorizeConfig &&
        authorizeConfig.icrc3Nonce !== undefined
      ) {
        await testAppPage
          .getByRole("textbox", { name: "ICRC-3 nonce (base64):" })
          .fill(toBase64(authorizeConfig.icrc3Nonce));
      }
      if (
        "attributes" in authorizeConfig &&
        authorizeConfig.attributes !== undefined &&
        authorizeConfig.attributes.length > 0
      ) {
        await testAppPage
          .getByRole("textbox", { name: "Request attributes:" })
          .fill(authorizeConfig.attributes.join("\n"));
      }
    } else {
      await testAppPage
        .getByRole("textbox", { name: "Identity Provider" })
        .fill(internetIdentityURL + "#authorize");
    }

    await expect(testAppPage.locator("#principal")).toBeHidden();

    const transport = authorizeConfig.transport ?? "window";
    switch (transport) {
      case "redirect": {
        // Redirect flow: clicking "Sign In" navigates THIS tab to /callback → II
        // /authorize. The authenticate interaction runs on the same page, and II
        // delivers the response back by navigation — no popup.
        await testAppPage.locator("#transport").selectOption("redirect");
        await testAppPage.getByRole("button", { name: "Sign In" }).click();

        await use(new AuthorizePage(testAppPage));

        // On the return load the homepage renders the principal.
        await expect(testAppPage.locator("#principal")).toBeVisible({
          timeout: 15_000,
        });
        break;
      }
      case "window": {
        // Window flow: clicking "Sign In" opens II in a new tab; the response
        // comes back over postMessage and the tab closes.
        const authPagePromise = testAppPage.context().waitForEvent("page");
        await testAppPage.getByRole("button", { name: "Sign In" }).click();
        const authPage = await authPagePromise;
        const closePromise = authPage.waitForEvent("close", {
          timeout: 15_000,
        });

        await use(new AuthorizePage(authPage));

        await closePromise;
        break;
      }
      default: {
        transport satisfies never;
        throw new Error(`Unhandled transport: ${String(transport)}`);
      }
    }
  },
  authorizedPrincipal: async ({ page, authorizeConfig }, use) => {
    const [testAppPage, authPage] = page.context().pages();
    if (authPage !== undefined) {
      await authPage.waitForEvent("close", { timeout: 15_000 });
    }
    // The redirect flow renders the principal only after navigating back to the
    // test app, so wait for it before reading (window flow already has it).
    if (authorizeConfig?.transport === "redirect") {
      await expect(testAppPage.locator("#principal")).toBeVisible({
        timeout: 15_000,
      });
    }

    const principal = await testAppPage.locator("#principal").textContent();
    if (principal === null) {
      return use(undefined);
    }

    await use(Principal.fromText(principal));
  },
  authorizedAttributes: async ({ page }, use) => {
    const [testAppPage, authPage] = page.context().pages();
    if (authPage !== undefined) {
      await authPage.waitForEvent("close", { timeout: 15_000 });
    }

    const attributes = await testAppPage
      .locator("#certifiedAttributes")
      .innerText();
    if (attributes === "") {
      return use(undefined);
    }

    await use(
      Object.fromEntries(
        attributes.split("\n").map((line) => {
          const [key, value] = line.split(": ");
          return [key, value];
        }),
      ),
    );
  },
  authorizedIcrc3Attributes: async ({ page, authorizeConfig }, use) => {
    const [testAppPage, authPage] = page.context().pages();
    if (authPage !== undefined) {
      await authPage.waitForEvent("close", { timeout: 15_000 });
    }
    // The redirect flow renders results only after navigating back to the test
    // app, so wait for the return (principal visible) before reading.
    if (authorizeConfig?.transport === "redirect") {
      await expect(testAppPage.locator("#principal")).toBeVisible({
        timeout: 15_000,
      });
    }

    // Tests that bypass the test_app entirely (e.g. the channel-error
    // test that goes straight to the authorize URL) won't have the
    // `#icrc3Attributes` element on the page. Bail out gracefully so
    // the fixture can still be destructured by a global afterEach
    // without forcing a 30s locator timeout.
    if ((await testAppPage.locator("#icrc3Attributes").count()) === 0) {
      return use(undefined);
    }
    const icrc3Attributes = await testAppPage
      .locator("#icrc3Attributes")
      .innerText();
    if (icrc3Attributes === "") {
      return use(undefined);
    }

    await use(JSON.parse(icrc3Attributes));
  },
  authorizedDelegation: async ({ page, authorizeConfig }, use) => {
    const [testAppPage, authPage] = page.context().pages();
    if (authPage !== undefined) {
      await authPage.waitForEvent("close", { timeout: 15_000 });
    }
    // The redirect flow renders the delegation only after navigating back to
    // the test app, so wait for the return (principal visible) before reading.
    if (authorizeConfig?.transport === "redirect") {
      await expect(testAppPage.locator("#principal")).toBeVisible({
        timeout: 15_000,
      });
    }

    const delegation = await testAppPage.locator("#delegation").innerText();
    // The test app writes a plain status string here (e.g. "Current identity is
    // not a DelegationIdentity") when no delegation was delivered, so parse
    // defensively: anything that isn't valid JSON means "no delegation" and
    // should fail the `toBeDefined()` assertion, not throw a SyntaxError from
    // inside the fixture.
    let parsed: Parameters<typeof use>[0];
    try {
      parsed = delegation === "" ? undefined : JSON.parse(delegation);
    } catch {
      parsed = undefined;
    }

    await use(parsed);
  },
  iiBackendCanisterId: async ({ browser }, use) => {
    // The II frontend canister rewrites every served HTML to embed
    // `<body data-canister-id="<II backend canister id>" ...>`. Visiting
    // the II URL in a throwaway context lets the fixture read the id
    // without depending on icp-cli or filesystem state.
    const context = await browser.newContext();
    const page = await context.newPage();
    try {
      await page.goto(II_URL);
      const id = await page.locator("body").getAttribute("data-canister-id");
      if (id === null || id === "") {
        throw new Error(
          "II frontend page didn't expose `data-canister-id` on <body>",
        );
      }
      await use(Principal.fromText(id));
    } finally {
      await page.close();
      await context.close();
    }
  },
  canisterEchoedIcrc3Attributes: async (
    { page, authorizedIcrc3Attributes, iiBackendCanisterId },
    use,
  ) => {
    if (authorizedIcrc3Attributes === undefined) {
      return use(undefined);
    }

    const [testAppPage] = page.context().pages();
    await testAppPage
      .getByRole("textbox", { name: "II canister id (signer)" })
      .fill(iiBackendCanisterId.toText());
    const humanReadableLocator = testAppPage.locator(
      "#canisterEchoedAttributes",
    );
    const rawLocator = testAppPage.locator("#canisterEchoedAttributesRaw");
    await testAppPage
      .getByRole("button", { name: "Send attributes to canister" })
      .click();
    await expect(humanReadableLocator).not.toHaveText("");
    await expect(humanReadableLocator).not.toHaveText("Loading...");
    const humanReadable = await humanReadableLocator.innerText();
    if (humanReadable.startsWith("Failed:")) {
      throw new Error(`Canister rejected the round-trip: ${humanReadable}`);
    }
    const raw = JSON.parse(await rawLocator.innerText()) as {
      signer: string | null;
      data: string;
    };
    await use(decodeIcrc3TextEntries(raw.data));
  },
});

// Parse a Candid-encoded ICRC-3 `Value` (Map of Text → Text) from a
// base64 string. Mirrors `consent.spec.ts` / `openid.spec.ts` so the
// authorize fixture stays self-contained.
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
  const dataBytes = Uint8Array.from(atob(base64Data), (c) => c.charCodeAt(0));
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
