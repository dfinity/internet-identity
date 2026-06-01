import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { test as base, type Page } from "@playwright/test";
import { createServer, type Server } from "node:http";
import { toBase64URL } from "../../../src/lib/utils/utils";
import { II_URL } from "../utils";

/** What the loopback server does on its next form POST. */
type CliOutcome = "success" | "identity-mismatch" | "error";

/**
 * Stands in for an ICP CLI binary. It:
 *  - runs a real loopback HTTP server the browser navigates to, so the test
 *    exercises the genuine top-level form-POST path (https page → http
 *    loopback) rather than mocking it;
 *  - generates an Ed25519 session keypair (the CLI's ephemeral key, base64url
 *    per the CLI URL contract);
 *  - discovers the authorize path the same way the real CLI does: it reads
 *    `/.well-known/cli-auth-config` and navigates to the advertised path,
 *    rather than hardcoding `/cli`;
 *  - on receiving the delegation, redirects the browser back to the `/cli`
 *    page with a `status` (mirroring the real CLI), so the frontend keeps
 *    ownership of the success/error/mismatch UI.
 *
 * `receivedDelegation` resolves with the delegation chain JSON once the flow
 * successfully posts it to the loopback server.
 */
export type CliFixture = {
  publicKey: string;
  nonce: string;
  callbackUrl: string;
  receivedDelegation: Promise<unknown>;
  /** Every delegation received so far, in order (for multi-post tests). */
  receivedDelegations: unknown[];
  /**
   * Sets what the loopback server does on its next form POST. Defaults to
   * "success". After serving an "identity-mismatch" redirect the server resets
   * to "success", mirroring the real CLI which keeps listening for a retry
   * with the correct identity.
   */
  setNextOutcome: (outcome: CliOutcome) => void;
  /**
   * Looks up `/.well-known/cli-auth-config` on the II origin and returns the
   * full authorize URL (advertised path + the CLI params in the fragment).
   * Navigates `page` to the II origin to perform the same-origin lookup.
   */
  resolveAuthorizeUrl: (
    page: Page,
    opts?: { domain?: string; ttlMinutes?: number; callbackUrl?: string },
  ) => Promise<string>;
};

export const test = base.extend<{ cli: CliFixture }>({
  // eslint-disable-next-line no-empty-pattern -- playwright fixtures require the destructure
  cli: async ({}, use) => {
    const identity = Ed25519KeyIdentity.generate();
    const publicKey = toBase64URL(
      new Uint8Array(identity.getPublicKey().toDer()),
    );
    // The single-use secret the real CLI puts in the URL fragment and the
    // frontend must echo back in its POST. The loopback stand-in rejects a
    // POST that doesn't carry it, mirroring the real CLI's nonce gate.
    const nonce = toBase64URL(crypto.getRandomValues(new Uint8Array(32)));

    let resolveDelegation: (body: unknown) => void = () => undefined;
    const receivedDelegation = new Promise<unknown>((resolve) => {
      resolveDelegation = resolve;
    });
    // Every delegation the loopback receives, in order — lets a test that posts
    // more than once (e.g. comparing derivation origins) read each one.
    const receivedDelegations: unknown[] = [];

    let nextOutcome: CliOutcome = "success";
    // The discovered login path the server redirects back to; set by
    // `resolveAuthorizeUrl` once it reads `/.well-known/cli-auth-config`.
    let loginPath = "/cli";

    const server: Server = createServer((req, res) => {
      // The delegation arrives as a top-level form-POST navigation, so there
      // is no CORS preflight to answer. Read the body, decide the outcome, and
      // redirect the browser back to the /cli page with a status — exactly
      // what the real loopback server does.
      let raw = "";
      req.on("data", (chunk) => (raw += chunk));
      req.on("end", () => {
        const url = new URL(loginPath, II_URL);
        const fragment = new URLSearchParams();

        if (nextOutcome === "identity-mismatch") {
          // Keep listening so the user can retry with the right identity:
          // re-supply the request params and let the next post succeed.
          nextOutcome = "success";
          fragment.set("public_key", publicKey);
          fragment.set("callback", callbackUrl);
          fragment.set("nonce", nonce);
          fragment.set("status", "identity-mismatch");
          url.hash = fragment.toString();
          res.writeHead(303, { Location: url.toString() });
          res.end();
          return;
        }

        if (nextOutcome === "error") {
          fragment.set("status", "error");
          url.hash = fragment.toString();
          res.writeHead(303, { Location: url.toString() });
          res.end();
          return;
        }

        const posted = new URLSearchParams(raw);
        if (posted.get("nonce") !== nonce) {
          // The frontend didn't echo the shared nonce — the real CLI would
          // reject this with 403. Redirect with an error status so the test
          // fails loudly instead of `receivedDelegation` hanging forever.
          fragment.set("status", "error");
          url.hash = fragment.toString();
          res.writeHead(303, { Location: url.toString() });
          res.end();
          return;
        }
        const delegation = posted.get("delegation");
        let parsed: unknown;
        try {
          parsed = delegation === null ? raw : JSON.parse(delegation);
        } catch {
          parsed = raw;
        }
        receivedDelegations.push(parsed);
        resolveDelegation(parsed);
        fragment.set("status", "success");
        url.hash = fragment.toString();
        res.writeHead(303, { Location: url.toString() });
        res.end();
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

    const setNextOutcome = (outcome: CliOutcome): void => {
      nextOutcome = outcome;
    };

    const resolveAuthorizeUrl = async (
      page: Page,
      opts: {
        domain?: string;
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
      loginPath = config.path;
      const fragment = new URLSearchParams();
      fragment.set("public_key", publicKey);
      fragment.set("callback", opts.callbackUrl ?? callbackUrl);
      fragment.set("nonce", nonce);
      if (opts.domain !== undefined) {
        fragment.set("domain", opts.domain);
      }
      if (opts.ttlMinutes !== undefined) {
        fragment.set("ttl", String(opts.ttlMinutes));
      }
      return `${II_URL}${config.path}#${fragment.toString()}`;
    };

    await use({
      publicKey,
      nonce,
      callbackUrl,
      receivedDelegation,
      receivedDelegations,
      setNextOutcome,
      resolveAuthorizeUrl,
    });

    await new Promise<void>((resolve) => server.close(() => resolve()));
  },
});
