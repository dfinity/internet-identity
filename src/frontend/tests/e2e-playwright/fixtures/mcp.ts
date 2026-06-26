import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { test as base, type Page } from "@playwright/test";
import { toBase64URL } from "../../../src/lib/utils/utils";
import { II_URL } from "../utils";

/** What the MCP server stand-in does on its next form POST. */
type McpOutcome = "success" | "error";

/**
 * A stand-in MCP server origin. There's no global `mcp_server_origin` config
 * any more: the connect flow takes the MCP server origin from the request's
 * callback, accepting any https origin (MCP connections are to remote servers).
 * This is just a valid https origin to drive the flow with; the `/mcp` page's
 * `form-action` CSP allows posting the delegation to it.
 */
const MCP_SERVER_ORIGIN = "https://mcp.id.ai";

/**
 * Stands in for a remote MCP server. Unlike the CLI loopback fixture there's no
 * real HTTP server: the callback is a public https origin, so the delegation
 * arrives as a top-level form-POST navigation to it. We intercept that
 * navigation with `page.route` (which catches it before the network, so no
 * server or DNS for `mcp.id.ai` is needed), read the posted delegation, and
 * fulfill a 303 redirect back to `/mcp` with a `status` — exactly what a real
 * MCP server would do.
 *
 * `receivedDelegation` resolves with the delegation chain JSON once the flow
 * successfully posts it.
 */
export type McpFixture = {
  publicKey: string;
  state: string;
  mcpOrigin: string;
  callbackUrl: string;
  receivedDelegation: Promise<unknown>;
  /** Every delegation received so far, in order (for multi-post tests). */
  receivedDelegations: unknown[];
  /** Sets what the MCP server stand-in does on its next form POST. */
  setNextOutcome: (outcome: McpOutcome) => void;
  /**
   * Trusts this fixture's MCP origin for the signed-up identity by driving the
   * Settings UI — the trusted server is now the identity's synced (on-chain)
   * config, not device-local storage, so there's no shortcut. Call after sign-up
   * (while on `/manage`) and before navigating to `/mcp`. Without it the connect
   * flow lands on the "untrusted" screen once the user authenticates, since each
   * identity trusts nothing by default.
   */
  trustServer: (page: Page) => Promise<void>;
  /**
   * Installs the form-POST interceptor on `page`. Must be called before the
   * flow submits the delegation. Reads the posted `delegation`/`state` and
   * redirects the browser back to `/mcp` with the configured outcome status.
   */
  installInterceptor: (page: Page) => Promise<void>;
  /** Builds the `/mcp` authorize URL with the request params in the fragment. */
  buildAuthorizeUrl: (opts: {
    app: string;
    ttlMinutes?: number;
    callbackUrl?: string;
  }) => string;
};

export const test = base.extend<{ mcp: McpFixture }>({
  // eslint-disable-next-line no-empty-pattern -- playwright fixtures require the destructure
  mcp: async ({}, use) => {
    const identity = Ed25519KeyIdentity.generate();
    const publicKey = toBase64URL(
      new Uint8Array(identity.getPublicKey().toDer()),
    );
    // Opaque value the MCP server puts in the request and the frontend echoes
    // back in its POST so the server can tie the delivery to the request.
    const state = toBase64URL(crypto.getRandomValues(new Uint8Array(32)));
    const callbackUrl = `${MCP_SERVER_ORIGIN}/callback`;

    let resolveDelegation: (body: unknown) => void = () => undefined;
    const receivedDelegation = new Promise<unknown>((resolve) => {
      resolveDelegation = resolve;
    });
    const receivedDelegations: unknown[] = [];

    let nextOutcome: McpOutcome = "success";
    const setNextOutcome = (outcome: McpOutcome): void => {
      nextOutcome = outcome;
    };

    const installInterceptor = async (page: Page): Promise<void> => {
      const redirectTo = (status: McpOutcome): string => {
        const url = new URL("/mcp", II_URL);
        url.hash = new URLSearchParams({ status }).toString();
        return url.toString();
      };

      await page.route(`${MCP_SERVER_ORIGIN}/**`, async (route) => {
        let location: string;
        const posted = new URLSearchParams(route.request().postData() ?? "");
        if (nextOutcome === "error" || posted.get("state") !== state) {
          // On an "error" outcome, or if the frontend didn't echo the request
          // state (a real MCP server would reject this), redirect back with an
          // error so the test fails loudly instead of `receivedDelegation`
          // hanging forever.
          location = redirectTo("error");
        } else {
          const delegation = posted.get("delegation");
          let parsed: unknown;
          try {
            parsed = delegation === null ? null : JSON.parse(delegation);
          } catch {
            parsed = delegation;
          }
          receivedDelegations.push(parsed);
          resolveDelegation(parsed);
          location = redirectTo("success");
        }
        await route.fulfill({ status: 303, headers: { location } });
      });
    };

    const trustServer = async (page: Page): Promise<void> => {
      // The trusted server is the identity's synced (on-chain) config, set via
      // Settings — so seed it the way a user would. Mock this origin's RFC 9728
      // metadata first so the Settings probe verifies fast and clean (the probe
      // is advisory; activation happens regardless). The narrow well-known
      // pattern doesn't collide with the callback interceptor's `/**` route.
      await page.route(
        `${MCP_SERVER_ORIGIN}/.well-known/oauth-protected-resource**`,
        (route) =>
          route.fulfill({
            status: 200,
            headers: {
              "access-control-allow-origin": "*",
              "content-type": "application/json",
            },
            body: JSON.stringify({
              authorization_servers: [MCP_SERVER_ORIGIN],
              resource: `${MCP_SERVER_ORIGIN}/mcp`,
            }),
          }),
      );

      // Reach Settings via in-app navigation (a full reload of an authenticated
      // route would drop the just-signed-up in-memory session). On mobile the
      // sidebar is collapsed behind a menu button, so open it first.
      const openMenu = page.getByRole("button", { name: "Open menu" });
      if (await openMenu.isVisible()) {
        await openMenu.click();
      }
      await page.locator('a[href="/manage/settings"]').click();
      await page.waitForURL(`${II_URL}/manage/settings`);
      // The URL box only appears once the master toggle is on; the remove button
      // appears once the trusted server is saved to the canister.
      await page.getByRole("switch", { name: "Trusted MCP server" }).check();
      await page.getByLabel("MCP server URL").fill(`${MCP_SERVER_ORIGIN}/mcp`);
      await page.getByRole("button", { name: "Trust this server" }).click();
      await page
        .getByRole("button", { name: "Remove this server" })
        .waitFor({ state: "visible" });
    };

    const buildAuthorizeUrl = (opts: {
      app: string;
      ttlMinutes?: number;
      callbackUrl?: string;
    }): string => {
      const fragment = new URLSearchParams();
      fragment.set("public_key", publicKey);
      fragment.set("callback", opts.callbackUrl ?? callbackUrl);
      fragment.set("state", state);
      fragment.set("app", opts.app);
      if (opts.ttlMinutes !== undefined) {
        fragment.set("ttl", String(opts.ttlMinutes));
      }
      return `${II_URL}/mcp#${fragment.toString()}`;
    };

    await use({
      publicKey,
      state,
      mcpOrigin: MCP_SERVER_ORIGIN,
      callbackUrl,
      receivedDelegation,
      receivedDelegations,
      setNextOutcome,
      trustServer,
      installInterceptor,
      buildAuthorizeUrl,
    });
  },
});
