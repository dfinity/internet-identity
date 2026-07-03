import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { test as base, type Page } from "@playwright/test";
import { toBase64URL } from "../../../src/lib/utils/utils";
import { II_URL } from "../utils";

/** What the MCP server stand-in does on its next callback request. */
type McpOutcome = "success" | "error";

/**
 * A stand-in MCP server origin. There's no global `mcp_server_origin` config
 * any more: the connect flow takes the MCP server origin from the request's
 * callback, accepting any https origin (MCP connections are to remote servers).
 * This is just a valid https origin to drive the flow with.
 */
const MCP_SERVER_ORIGIN = "https://mcp.id.ai";

/** The completion notification the connect flow POSTs once the session is
 *  registered: the state echo plus the grant expiration (ns since epoch, as a
 *  decimal string — the value overflows JSON numbers). */
export type McpCompletion = { state: string; expiration: string };

/**
 * Stands in for a remote MCP server. Unlike the CLI loopback fixture there's
 * no real HTTP server: the callback is a public https origin, and the connect
 * flow talks to it with JSON `fetch`es. We intercept those with `page.route`
 * (which catches them before the network, so no server or DNS for `mcp.id.ai`
 * is needed) and answer exactly like a real MCP server would: the key request
 * (`{state}`) gets this connection's session public key, and the completion
 * notification (`{state, expiration}`) gets a 200. A request with a state the
 * server never issued gets a 403 — nothing is registered then.
 *
 * `completion` resolves with the completion body once the flow reports the
 * session registered.
 */
export type McpFixture = {
  publicKey: string;
  state: string;
  mcpOrigin: string;
  callbackUrl: string;
  completion: Promise<McpCompletion>;
  /** Every completion received so far, in order (for multi-connect tests). */
  completions: McpCompletion[];
  /** Sets what the MCP server stand-in does on its next callback request. */
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
   * Installs the callback interceptor on `page`. Must be called before the
   * flow contacts the server (i.e. before "Allow access").
   */
  installInterceptor: (page: Page) => Promise<void>;
  /** Builds the `/mcp` authorize URL with the request params in the fragment. */
  buildAuthorizeUrl: (opts: {
    app: string;
    ttlSeconds?: number;
    callbackUrl?: string;
  }) => string;
};

// The connect flow's callback requests are cross-origin JSON `fetch`es, so the
// stand-in must answer the CORS preflight and mark its responses readable —
// exactly what a real MCP server has to do.
const CORS_HEADERS = {
  "access-control-allow-origin": "*",
  "access-control-allow-methods": "POST",
  "access-control-allow-headers": "content-type",
};

export const test = base.extend<{ mcp: McpFixture }>({
  // eslint-disable-next-line no-empty-pattern -- playwright fixtures require the destructure
  mcp: async ({}, use) => {
    const identity = Ed25519KeyIdentity.generate();
    const publicKey = toBase64URL(
      new Uint8Array(identity.getPublicKey().toDer()),
    );
    // Opaque value the MCP server puts in the request and the frontend sends
    // back in its callback requests so the server can tie them to the request
    // it started.
    const state = toBase64URL(crypto.getRandomValues(new Uint8Array(32)));
    const callbackUrl = `${MCP_SERVER_ORIGIN}/callback`;

    let resolveCompletion: (body: McpCompletion) => void = () => undefined;
    const completion = new Promise<McpCompletion>((resolve) => {
      resolveCompletion = resolve;
    });
    const completions: McpCompletion[] = [];

    let nextOutcome: McpOutcome = "success";
    const setNextOutcome = (outcome: McpOutcome): void => {
      nextOutcome = outcome;
    };

    const installInterceptor = async (page: Page): Promise<void> => {
      await page.route(`${MCP_SERVER_ORIGIN}/**`, async (route) => {
        const request = route.request();
        if (request.method() === "OPTIONS") {
          // CORS preflight for the JSON POSTs.
          await route.fulfill({ status: 204, headers: CORS_HEADERS });
          return;
        }
        let body: Record<string, unknown> = {};
        try {
          body = JSON.parse(request.postData() ?? "{}") as Record<
            string,
            unknown
          >;
        } catch {
          // Leave `body` empty; the state check below rejects the request.
        }
        if (nextOutcome === "error" || body.state !== state) {
          // On an "error" outcome, or a state this server never issued (a real
          // MCP server must reject that), answer 403 — the flow errors out and
          // nothing is registered.
          await route.fulfill({ status: 403, headers: CORS_HEADERS });
          return;
        }
        if (typeof body.expiration === "string") {
          // Completion notification: the session is registered.
          const received = { state, expiration: body.expiration };
          completions.push(received);
          resolveCompletion(received);
          await route.fulfill({
            status: 200,
            headers: { ...CORS_HEADERS, "content-type": "application/json" },
            body: "{}",
          });
          return;
        }
        // Key request: serve this connection's session public key.
        await route.fulfill({
          status: 200,
          headers: { ...CORS_HEADERS, "content-type": "application/json" },
          body: JSON.stringify({ public_key: publicKey }),
        });
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
      ttlSeconds?: number;
      callbackUrl?: string;
    }): string => {
      const fragment = new URLSearchParams();
      fragment.set("callback", opts.callbackUrl ?? callbackUrl);
      fragment.set("state", state);
      fragment.set("app", opts.app);
      if (opts.ttlSeconds !== undefined) {
        fragment.set("ttl", String(opts.ttlSeconds));
      }
      return `${II_URL}/mcp#${fragment.toString()}`;
    };

    await use({
      publicKey,
      state,
      mcpOrigin: MCP_SERVER_ORIGIN,
      callbackUrl,
      completion,
      completions,
      setNextOutcome,
      trustServer,
      installInterceptor,
      buildAuthorizeUrl,
    });
  },
});
