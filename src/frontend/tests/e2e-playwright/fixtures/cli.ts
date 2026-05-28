import { Ed25519KeyIdentity } from "@icp-sdk/core/identity";
import { test as base } from "@playwright/test";
import { createServer, type Server } from "node:http";
import { toHex } from "../../../src/lib/utils/utils";

/**
 * Simulates the loopback HTTP server an ICP CLI binary stands up to receive
 * the delegation back from `id.ai/cli`. The fixture:
 *  - generates an Ed25519 session keypair (the CLI's ephemeral keypair),
 *  - starts a 127.0.0.1 server that captures the POST body II sends,
 *  - exposes `publicKeyHex` + `callbackUrl` to assemble the `id.ai/cli` URL.
 *
 * On test teardown the server is closed.
 */
export type CliFixture = {
  publicKeyHex: string;
  callbackUrl: string;
  /** Promise that resolves to the delegation chain JSON the CLI received. */
  receivedDelegation: Promise<unknown>;
};

export const test = base.extend<{ cli: CliFixture }>({
  // eslint-disable-next-line no-empty-pattern -- playwright fixtures require the destructure
  cli: async ({}, use) => {
    const identity = Ed25519KeyIdentity.generate();
    const publicKeyHex = toHex(new Uint8Array(identity.getPublicKey().toDer()));

    let resolveDelegation: (body: unknown) => void = () => undefined;
    const receivedDelegation = new Promise<unknown>((resolve) => {
      resolveDelegation = resolve;
    });

    const server: Server = createServer((req, res) => {
      let raw = "";
      req.on("data", (chunk) => (raw += chunk));
      req.on("end", () => {
        try {
          resolveDelegation(JSON.parse(raw));
        } catch {
          resolveDelegation(raw);
        }
        res.statusCode = 200;
        res.setHeader("Access-Control-Allow-Origin", "*");
        res.end("ok");
      });
    });
    // `listen(0, …)` asks the OS for an unused port, so concurrent fixture
    // instances never collide on a hardcoded port.
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", () => resolve()),
    );
    const address = server.address();
    if (address === null || typeof address === "string") {
      throw new Error(
        "Loopback CLI fixture: server.address() not an AddressInfo",
      );
    }
    const callbackUrl = `http://127.0.0.1:${address.port}/callback`;

    await use({ publicKeyHex, callbackUrl, receivedDelegation });

    await new Promise<void>((resolve) => server.close(() => resolve()));
  },
});
