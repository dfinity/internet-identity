import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { test as base, type Page } from "@playwright/test";
import { toBase64URL } from "../../../src/lib/utils/utils";
import { storeLocalStorageKey } from "../../../src/lib/constants/store.constants";
import { II_URL } from "../utils";

/** What the MCP server stand-in does on its next form POST. */
type McpOutcome = "success" | "error";

/**
 * A stand-in MCP server origin. There's no global `mcp_server_origin` config
 * any more: the connect flow takes the MCP server origin from the request's
 * callback, accepting any https origin (or http loopback). This is just a valid
 * https origin to drive the flow with; the `/mcp` page's `form-action` CSP
 * allows posting the delegation to it.
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
   * Seeds the device-local trusted-servers allowlist so the connect flow treats
   * this fixture's MCP origin as trusted for the already-signed-up identity.
   * Call after sign-up and before navigating to `/mcp`. Without it the flow
   * lands on the "untrusted" screen, since each identity trusts nothing by
   * default.
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
      // Read just the signed-up identity number(s) out of the last-used store.
      const identityNumbers = await page.evaluate((lastUsedKey) => {
        const raw = localStorage.getItem(lastUsedKey);
        if (raw === null) {
          return [] as string[];
        }
        const parsed = JSON.parse(raw) as { data?: Record<string, unknown> };
        return Object.keys(parsed.data ?? {});
      }, storeLocalStorageKey.LastUsedIdentities);
      if (identityNumbers.length === 0) {
        throw new Error("trustServer: no identity number in localStorage");
      }
      // Build the trusted-server envelope in Node (matching writableStored's
      // `{ data, version }` shape — tests sign up exactly one identity, and the
      // store holds a single trusted URL per identity) and write the finished
      // string back. Keeping the read and the write in separate evaluates means
      // the page-side write just persists a known constant, not data read from
      // storage in the same step.
      const trusted: Record<string, string> = {};
      for (const num of identityNumbers) {
        trusted[num] = `${MCP_SERVER_ORIGIN}/mcp`;
      }
      const value = JSON.stringify({ data: trusted, version: 2 });
      await page.evaluate(
        ({ trustedKey, value }) => localStorage.setItem(trustedKey, value),
        { trustedKey: storeLocalStorageKey.McpTrustedServers, value },
      );
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
