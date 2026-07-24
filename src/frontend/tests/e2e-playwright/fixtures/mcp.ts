import {
  DelegationChain,
  DelegationIdentity,
  Ed25519KeyIdentity,
} from "@icp-sdk/core/identity";
import { Actor, HttpAgent } from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import { Agent as UndiciAgent } from "undici";
import { test as base, type Page } from "@playwright/test";
import { readCanisterId } from "@dfinity/internet-identity-vite-plugins/utils";
import { idlFactory as internet_identity_idl } from "$lib/generated/internet_identity_idl";
import type {
  _SERVICE,
  Permissions,
} from "$lib/generated/internet_identity_types";
import { toBase64URL } from "../../../src/lib/utils/utils";
import { AUTH_CALLBACKS_PATH } from "../../../src/lib/utils/authCallbacks";
import { holdToConfirm, II_URL } from "../utils";
import { DEFAULT_HOST } from "./identity";

/** What the MCP server stand-in does on its next redemption. */
type McpOutcome = "success" | "error";

/**
 * A stand-in MCP server origin. There's no global `mcp_server_origin` config
 * any more: the connect flow takes the MCP server origin from the request's
 * callback, accepting any https origin (MCP connections are to remote servers).
 * This is just a valid https origin to drive the flow with.
 */
const MCP_SERVER_ORIGIN = "https://mcp.id.ai";

/** The server-chosen path of its connect callback page — any path works, as
 *  long as the server declares the full URL in its auth-callback allow-list
 *  (see `AUTH_CALLBACKS_PATH`). Deliberately not a well-known path: the server
 *  owns its own routing under the declared-callbacks model. */
const CONNECT_CALLBACK_PATH = "/mcp/connect";

/** A fixture-internal endpoint the connect page POSTs the delivered fragment
 *  to. Same-origin with the server (no CORS preflight); it stands in for the
 *  server's backend, which holds `X`'s private key and redeems the chain. */
const REDEEM_PATH = "/__ii_mcp_redeem";

/** The II backend canister (exposes `mcp_register_v2`). Read once at module
 *  load, like `test_app` in `utils.ts`. */
const II_CANISTER_ID = Principal.fromText(
  readCanisterId({ canisterName: "internet_identity" }),
);

/** Self-signed-cert tolerant fetch for the redeeming agent — same setup as
 *  `utils.ts` / `emailRecovery.ts`. */
const insecureUndici = new UndiciAgent({
  connect: { rejectUnauthorized: false },
});
const insecureFetch: typeof fetch = (url, options = {}) =>
  // Spread the caller's options first, then pin the dispatcher, so this
  // fixture's self-signed-tolerant agent is authoritative and can't be
  // silently overridden (defensive — the callers here never pass a dispatcher).
  fetch(url, { ...options, dispatcher: insecureUndici } as RequestInit);

const permissionsString = (permissions: Permissions): string =>
  "queries" in permissions ? "queries" : "all";

/** The outcome the connect reports once the server redeems the registration
 *  delegation: the state echo, the session grant's expiration (ns since epoch,
 *  as a decimal string — the value overflows JSON numbers), and the session's
 *  access level (`permissions`: "queries" = read-only, "all" = full), all read
 *  off `mcp_register_v2`'s response. */
export type McpCompletion = {
  state: string;
  expiration: string;
  permissions: string;
};

/**
 * Stands in for a remote MCP server in the *registration-delegation* connect
 * flow. The server generates two keys per browser session: a per-connect
 * registration key `X` (its public key rides the connect link) and the
 * long-lived session key `S` it wants bound to the anchor. It declares its
 * connect callback in the auth-callback allow-list it hosts at
 * `/.well-known/ii-auth-callbacks`; II exact-matches the link's callback
 * against that list, mints a short-lived registration chain — a canister-signed
 * `P_reg -> Y` hop to a browser-held ephemeral key, extended browser-side with
 * a `Y -> X` hop — and hands it to the declared callback over a top-level
 * navigation (only the chain and the `state` echo in the URL fragment; no
 * consent is delivered — the canister recovers it all from the entry). There's
 * no real HTTP server: `page.route` intercepts the navigation and serves a
 * page that reads the fragment and POSTs it to a fixture endpoint; that
 * handler — holding `X`'s private key — reconstructs the chain and redeems it
 * by calling `mcp_register_v2(pub(S))` as the server would, with no consent
 * arguments, then resolves `completion`.
 */
export type McpFixture = {
  /** The server's registration public key `X` (DER, base64url) — what rides
   *  the connect link. */
  registrationKey: string;
  state: string;
  mcpOrigin: string;
  callbackUrl: string;
  /** Where the server's connect page sends the browser after a successful
   *  redemption once `enableFinishRedirect()` is called: a landing page on its
   *  own origin, like a real server completing its own (e.g. OAuth) flow. */
  finishUrl: string;
  completion: Promise<McpCompletion>;
  /** Every completion received so far, in order (for multi-connect tests). */
  completions: McpCompletion[];
  /** Sets what the server stand-in does on its next redemption. */
  setNextOutcome: (outcome: McpOutcome) => void;
  /** Makes the server's connect page hand the tab back to the server (at
   *  `finishUrl`) after a successful redemption, instead of just showing its
   *  connected state. Off by default. */
  enableFinishRedirect: () => void;
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
   * Installs the server stand-in on `page`: the auth-callback allow-list, the
   * declared connect callback (serves the fragment-reading page), the redeem
   * endpoint, and the finish landing. Must be called before the flow delivers
   * the delegation (i.e. before "Allow access").
   */
  installInterceptor: (page: Page) => Promise<void>;
  /** Builds the `/mcp` authorize URL with the request params in the fragment. */
  buildAuthorizeUrl: (opts: {
    app: string;
    ttlSeconds?: number;
    callbackUrl?: string;
  }) => string;
};

// The auth-callback allow-list is fetched cross-origin by the II frontend, so
// its response must carry CORS headers to be readable — part of the server's
// contract. The redeem POST is same-origin with the served connect page, so it
// isn't preflighted; the headers are applied to the fixture's JSON responses
// defensively either way.
const CORS_HEADERS = {
  "access-control-allow-origin": "*",
  "access-control-allow-methods": "GET, POST",
  "access-control-allow-headers": "content-type",
};

/** The page served at the declared connect callback. Reads the delivered
 *  delegation + state from the fragment (never sent to the server in the HTTP
 *  request) and ships them to the redeem endpoint; no consent is delivered —
 *  the canister recovers it all from the entry. On success it either lands on
 *  its "connected" state or, if the server asked for it, navigates onward to
 *  its own finish URL. */
const connectPageHtml = (redeemPath: string): string => `<!doctype html>
<meta charset="utf-8" />
<title>MCP connect</title>
<h1 id="status">Connecting…</h1>
<script>
  (async () => {
    const el = document.getElementById("status");
    const params = new URLSearchParams(location.hash.slice(1));
    const delegation = params.get("delegation");
    const state = params.get("state");
    // Clear the fragment once parsed (keeping the query string — the declared
    // callback may carry one) so the delegation doesn't linger in the address
    // bar / history — what the server guide tells real servers to do.
    history.replaceState(null, "", location.pathname + location.search);
    if (!delegation || !state) {
      el.textContent = "Missing delegation";
      return;
    }
    try {
      const res = await fetch(${JSON.stringify(redeemPath)}, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({ delegation, state }),
      });
      const data = await res.json().catch(() => ({}));
      if (res.ok && data.ok) {
        if (data.finishUrl) {
          location.assign(data.finishUrl);
          return;
        }
        el.textContent = "Connected";
      } else {
        el.textContent = "Connection failed";
      }
    } catch {
      el.textContent = "Connection failed";
    }
  })();
</script>`;

export const test = base.extend<{ mcp: McpFixture }>({
  // eslint-disable-next-line no-empty-pattern -- playwright fixtures require the destructure
  mcp: async ({}, use) => {
    // The server's two per-session keys: X (registration key, public part rides
    // the link) and S (the long-lived session key it wants bound).
    const registrationIdentity = Ed25519KeyIdentity.generate();
    const sessionIdentity = Ed25519KeyIdentity.generate();
    const registrationKey = toBase64URL(
      new Uint8Array(registrationIdentity.getPublicKey().toDer()),
    );
    const sessionKey = new Uint8Array(sessionIdentity.getPublicKey().toDer());

    // Opaque value the server puts in the link and the connect page echoes back
    // with the delegation, so the server can tie it to the connect it started.
    const state = toBase64URL(crypto.getRandomValues(new Uint8Array(32)));
    // The link's callback: the server's connect page, declared in the
    // allow-list it hosts at /.well-known/ii-auth-callbacks. II honours it
    // only after exact-matching it against that list.
    const callbackUrl = `${MCP_SERVER_ORIGIN}${CONNECT_CALLBACK_PATH}`;
    const finishUrl = `${MCP_SERVER_ORIGIN}/oauth/finish?sid=fixture`;

    let finishRedirect = false;
    const enableFinishRedirect = (): void => {
      finishRedirect = true;
    };

    let resolveCompletion: (body: McpCompletion) => void = () => undefined;
    const completion = new Promise<McpCompletion>((resolve) => {
      resolveCompletion = resolve;
    });
    const completions: McpCompletion[] = [];

    let nextOutcome: McpOutcome = "success";
    const setNextOutcome = (outcome: McpOutcome): void => {
      nextOutcome = outcome;
    };

    // Redeem the delivered `P_reg -> Y -> X` chain exactly as the server would:
    // reconstruct the chain, sign as X (which the server holds), and bind S via
    // `mcp_register_v2(pub(S))` — no consent arguments. `caller()` is `P_reg`;
    // the backend recovers the whole consent (anchor, read-only, TTL) from the
    // registration entry keyed by `caller()`, so the server neither delivers
    // nor passes any of it. The access level comes back in the result.
    const redeem = async (delegationJson: string): Promise<McpCompletion> => {
      const chain = DelegationChain.fromJSON(JSON.parse(delegationJson));
      const identity = DelegationIdentity.fromDelegation(
        registrationIdentity,
        chain,
      );
      const agent = await HttpAgent.create({
        host: DEFAULT_HOST,
        shouldFetchRootKey: true,
        verifyQuerySignatures: false,
        fetch: insecureFetch,
        identity,
      });
      const actor = Actor.createActor<_SERVICE>(internet_identity_idl, {
        agent,
        canisterId: II_CANISTER_ID,
      });
      const result = await actor.mcp_register_v2(sessionKey);
      if ("Err" in result) {
        throw new Error(result.Err);
      }
      return {
        state,
        expiration: result.Ok.expiration.toString(),
        permissions: permissionsString(result.Ok.permissions),
      };
    };

    const installInterceptor = async (page: Page): Promise<void> => {
      await page.route(`${MCP_SERVER_ORIGIN}/**`, async (route) => {
        const request = route.request();
        const { pathname } = new URL(request.url());

        if (request.method() === "OPTIONS") {
          await route.fulfill({ status: 204, headers: CORS_HEADERS });
          return;
        }

        // The connect page delivers the fragment here. Redeem it (the server's
        // backend role), or fail on an "error" outcome / a state it never issued.
        if (request.method() === "POST" && pathname === REDEEM_PATH) {
          if (nextOutcome === "error") {
            await route.fulfill({ status: 500, headers: CORS_HEADERS });
            return;
          }
          let body: {
            delegation?: unknown;
            state?: unknown;
          } = {};
          try {
            body = JSON.parse(request.postData() ?? "{}") as typeof body;
          } catch {
            // Leave `body` empty; the checks below reject the request.
          }
          if (typeof body.delegation !== "string" || body.state !== state) {
            await route.fulfill({ status: 403, headers: CORS_HEADERS });
            return;
          }
          try {
            const received = await redeem(body.delegation);
            completions.push(received);
            resolveCompletion(received);
            await route.fulfill({
              status: 200,
              headers: { ...CORS_HEADERS, "content-type": "application/json" },
              body: JSON.stringify({
                ok: true,
                finishUrl: finishRedirect ? finishUrl : null,
              }),
            });
          } catch (error) {
            await route.fulfill({
              status: 502,
              headers: { ...CORS_HEADERS, "content-type": "application/json" },
              body: JSON.stringify({ ok: false, error: String(error) }),
            });
          }
          return;
        }

        if (request.method() === "GET" && pathname === AUTH_CALLBACKS_PATH) {
          // The server-declared auth-callback allow-list: II fetches it and
          // exact-matches the link's callback against it before delivering
          // anything. Served with CORS (a cross-origin JSON GET) and the
          // application/json content type II requires.
          await route.fulfill({
            status: 200,
            headers: { ...CORS_HEADERS, "content-type": "application/json" },
            body: JSON.stringify({ callbacks: [callbackUrl] }),
          });
          return;
        }

        if (request.method() === "GET" && pathname === CONNECT_CALLBACK_PATH) {
          // The declared connect callback: II delivers the delegation here.
          await route.fulfill({
            status: 200,
            headers: { "content-type": "text/html" },
            body: connectPageHtml(REDEEM_PATH),
          });
          return;
        }

        if (request.method() === "GET" && request.url() === finishUrl) {
          // The server's own landing page after it finishes its flow.
          await route.fulfill({
            status: 200,
            headers: { "content-type": "text/html" },
            body: "<h1>Connection complete</h1>",
          });
          return;
        }

        await route.fulfill({ status: 404 });
      });
    };

    const trustServer = async (page: Page): Promise<void> => {
      // The trusted server is the identity's synced (on-chain) config, set via
      // Settings — so seed it the way a user would. Mock this origin's RFC 9728
      // metadata first so the Settings probe verifies fast and clean (the probe
      // is advisory; activation happens regardless). Registered after the
      // connect interceptor so this narrower route wins for its exact path.
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
      await page.getByRole("switch", { name: "AI access" }).click();
      await page.getByLabel("MCP server URL").fill(`${MCP_SERVER_ORIGIN}/mcp`);
      await holdToConfirm(page, "Hold to continue");
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
      fragment.set("registration_key", registrationKey);
      fragment.set("callback", opts.callbackUrl ?? callbackUrl);
      fragment.set("state", state);
      fragment.set("app", opts.app);
      if (opts.ttlSeconds !== undefined) {
        fragment.set("ttl", String(opts.ttlSeconds));
      }
      return `${II_URL}/mcp#${fragment.toString()}`;
    };

    await use({
      registrationKey,
      state,
      mcpOrigin: MCP_SERVER_ORIGIN,
      callbackUrl,
      finishUrl,
      completion,
      completions,
      setNextOutcome,
      enableFinishRedirect,
      trustServer,
      installInterceptor,
      buildAuthorizeUrl,
    });
  },
});
