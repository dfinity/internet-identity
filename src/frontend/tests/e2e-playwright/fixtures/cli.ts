import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { test as base, type Page } from "@playwright/test";
import { toBase64URL } from "../../../src/lib/utils/utils";

/**
 * Stands in for the loopback server an ICP CLI binary runs to receive the
 * delegation from `id.ai/cli`. A real HTTP server can't be used here: the
 * e2e browser launches with `--host-resolver-rules=MAP * localhost:5173`,
 * which remaps every host (including a `127.0.0.1:<port>` callback) to the
 * dev server, so the browser could never reach a real loopback server.
 * Instead we intercept the callback request in the browser via
 * `page.route`, which runs before host resolution.
 *
 * The fixture generates an Ed25519 session keypair (the CLI's ephemeral
 * key, base64url-encoded per the CLI URL contract) and exposes
 * `captureDelegation(page)` to install the interceptor and await the body.
 */
export type CliFixture = {
  publicKey: string;
  callbackUrl: string;
  /**
   * Installs a route interceptor for {@link callbackUrl} on the page and
   * returns a promise that resolves with the parsed JSON the CLI flow POSTs
   * (the delegation chain). Call before triggering the authorize action.
   */
  captureDelegation: (page: Page) => Promise<unknown>;
};

// Arbitrary loopback URL — no server listens here; the request is intercepted
// in the browser. It only needs to pass the route's loopback validation.
const CALLBACK_URL = "http://127.0.0.1:54321/callback";

export const test = base.extend<{ cli: CliFixture }>({
  // eslint-disable-next-line no-empty-pattern -- playwright fixtures require the destructure
  cli: async ({}, use) => {
    const identity = Ed25519KeyIdentity.generate();
    const publicKey = toBase64URL(
      new Uint8Array(identity.getPublicKey().toDer()),
    );

    const captureDelegation = (page: Page): Promise<unknown> =>
      new Promise((resolve) => {
        void page.route(CALLBACK_URL, async (route) => {
          const request = route.request();
          // The application/json POST is preceded by a CORS preflight;
          // answer it without resolving, then capture the real POST.
          if (request.method() === "OPTIONS") {
            await route.fulfill({
              status: 204,
              headers: {
                "Access-Control-Allow-Origin": "*",
                "Access-Control-Allow-Methods": "POST, OPTIONS",
                "Access-Control-Allow-Headers": "Content-Type",
              },
            });
            return;
          }
          const body = request.postData();
          await route.fulfill({
            status: 200,
            headers: { "Access-Control-Allow-Origin": "*" },
            body: "ok",
          });
          try {
            resolve(JSON.parse(body ?? ""));
          } catch {
            resolve(body);
          }
        });
      });

    await use({ publicKey, callbackUrl: CALLBACK_URL, captureDelegation });
  },
});
