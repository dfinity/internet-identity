import { verifyIcSignature } from "@dfinity/sig-verifier-js/sig_verifier_js";
import type { IncomingMessage, ServerResponse } from "http";
import type { Plugin, ViteDevServer } from "vite";
import { defineConfig } from "vite";

export async function createChallenge(): Promise<string> {
  // TODO: generate random challenge, store & add expiry
  return "YSBjaGFsbGVuZ2UsIGkuZS4gYSBzdHJpbmcgb2YgYXQgbGVhc3QgMzIgYnl0ZXM=";
}

export async function verifyChallenge() {
  const res = await verifyIcSignature(
    // TODO: use delegation
    new Uint8Array(),
    new Uint8Array(),
    new Uint8Array(),
    new Uint8Array()
  );

  // TODO: check result
}

const handleChallenge = async (req: IncomingMessage, res: ServerResponse) => {
  const challenge = await createChallenge();
  res.statusCode = 200;
  res.end(JSON.stringify({ challenge }));
};

const handleVerify = async (req: IncomingMessage, res: ServerResponse) => {
  res.statusCode = 200;
  // TODO: verify challenge & add cookie here on success
  res.end(JSON.stringify({ status: "ok" }));
};

const backendPlugin = (): Plugin => ({
  name: "backend-plugin",
  configureServer(server: ViteDevServer) {
    server.middlewares.use(async (req, res, next) => {
      if (req.url === "/challenge") {
        return handleChallenge(req, res);
      }
      if (req.url === "/verify") {
        return handleVerify(req, res);
      }

      return next();
    });
  },
});

export default defineConfig({
  plugins: [backendPlugin()],
  server: { port: 5178 },
});
