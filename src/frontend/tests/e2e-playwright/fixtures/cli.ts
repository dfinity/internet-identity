import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { test as base, type Page } from "@playwright/test";
import { createServer, type Server } from "node:http";
import { toBase64URL } from "../../../src/lib/utils/utils";
import { II_URL } from "../utils";

/**
 * Stands in for an ICP CLI binary. It:
 *  - runs a real loopback HTTP server the browser connects to, so the test
 *    exercises the genuine cross-origin path — CORS preflight, mixed content
 *    (https page → http loopback), and Private Network Access — rather than
 *    mocking it;
 *  - generates an Ed25519 session keypair (the CLI's ephemeral key, base64url
 *    per the CLI URL contract);
 *  - discovers the authorize path the same way the real CLI does: it reads
 *    `/.well-known/cli-auth-config` and navigates to the advertised path,
 *    rather than hardcoding `/cli`.
 *
 * `receivedDelegation` resolves with the delegation chain JSON once the flow
 * POSTs it to the loopback server.
 */
export type CliFixture = {
  publicKey: string;
  callbackUrl: string;
  receivedDelegation: Promise<unknown>;
  /**
   * Looks up `/.well-known/cli-auth-config` on the II origin and returns the
   * full authorize URL (advertised path + the CLI params in the fragment).
   * Navigates `page` to the II origin to perform the same-origin lookup.
   */
  resolveAuthorizeUrl: (
    page: Page,
    opts?: { appHost?: string; ttlMinutes?: number; callbackUrl?: string },
  ) => Promise<string>;
};

export const test = base.extend<{ cli: CliFixture }>({
  // eslint-disable-next-line no-empty-pattern -- playwright fixtures require the destructure
  cli: async ({}, use) => {
    const identity = Ed25519KeyIdentity.generate();
    const publicKey = toBase64URL(
      new Uint8Array(identity.getPublicKey().toDer()),
    );

    let resolveDelegation: (body: unknown) => void = () => undefined;
    const receivedDelegation = new Promise<unknown>((resolve) => {
      resolveDelegation = resolve;
    });

    const server: Server = createServer((req, res) => {
      const origin = req.headers.origin ?? "*";
      // The application/json POST is a non-simple cross-origin request, so the
      // browser sends an OPTIONS preflight first. A real CLI loopback server
      // must answer it with CORS headers — and, because the request goes from
      // a public origin to a loopback (local) address, the Private Network
      // Access header too. Answer the preflight without resolving.
      if (req.method === "OPTIONS") {
        res.writeHead(204, {
          "Access-Control-Allow-Origin": origin,
          "Access-Control-Allow-Methods": "POST, OPTIONS",
          "Access-Control-Allow-Headers": "Content-Type",
          "Access-Control-Allow-Private-Network": "true",
        });
        res.end();
        return;
      }
      let raw = "";
      req.on("data", (chunk) => (raw += chunk));
      req.on("end", () => {
        res.writeHead(200, { "Access-Control-Allow-Origin": origin });
        res.end("ok");
        try {
          resolveDelegation(JSON.parse(raw));
        } catch {
          resolveDelegation(raw);
        }
      });
    });

    // `listen(0, …)` lets the OS pick an unused port, so concurrent fixture
    // instances never collide.
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", () => resolve()),
    );
    const address = server.address();
    if (address === null || typeof address === "string") {
      throw new Error("Loopback CLI fixture: server.address() not AddressInfo");
    }
    const callbackUrl = `http://127.0.0.1:${address.port}/callback`;

    const resolveAuthorizeUrl = async (
      page: Page,
      opts: {
        appHost?: string;
        ttlMinutes?: number;
        callbackUrl?: string;
      } = {},
    ): Promise<string> => {
      // Same-origin fetch from the II page — the only reliable way to reach
      // the dev-server-served canister in the e2e harness (the browser's
      // host-resolver rules don't apply to Playwright's request context).
      // Navigate to the II origin only if we're not already on it, to avoid
      // a redundant navigation when the caller is mid-flow on id.ai.
      if (new URL(page.url()).origin !== new URL(II_URL).origin) {
        await page.goto(II_URL);
      }
      const body = await page.evaluate(() =>
        fetch("/.well-known/cli-auth-config").then((r) => r.text()),
      );
      const config: unknown = JSON.parse(body);
      if (
        typeof config !== "object" ||
        config === null ||
        !("path" in config) ||
        typeof config.path !== "string"
      ) {
        throw new Error("cli-auth-config did not advertise a string path");
      }
      const fragment = new URLSearchParams();
      fragment.set("public_key", publicKey);
      fragment.set("callback", opts.callbackUrl ?? callbackUrl);
      if (opts.appHost !== undefined) {
        fragment.set("app", opts.appHost);
      }
      if (opts.ttlMinutes !== undefined) {
        fragment.set("ttl", String(opts.ttlMinutes));
      }
      return `${II_URL}${config.path}#${fragment.toString()}`;
    };

    await use({
      publicKey,
      callbackUrl,
      receivedDelegation,
      resolveAuthorizeUrl,
    });

    await new Promise<void>((resolve) => server.close(() => resolve()));
  },
});
