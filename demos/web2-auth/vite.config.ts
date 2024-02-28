import { validateDelegationAndGetPrincipal } from "@dfinity/sig-verifier-js/sig_verifier_js";
import { IncomingMessage, ServerResponse } from "http";
import type { Plugin, ViteDevServer } from "vite";
import { defineConfig } from "vite";

export async function createChallenge(): Promise<string> {
  // TODO: generate random challenge, store & add expiry
  return "YSBjaGFsbGVuZ2UsIGkuZS4gYSBzdHJpbmcgb2YgYXQgbGVhc3QgMzIgYnl0ZXM=";
}

type Delegation = {
  delegation: {
    pubkey: string;
    expiration: string;
  };
  signature: string;
};

type VerifyData = {
  challenge: string;
  authMethod: string;
  delegationIdentity: {
    kind: string;
    delegations: Delegation[];
    userPublicKey: string;
  };
};

const ROOT_PUBLIC_KEY_RAW = new Uint8Array([
  0x81, 0x4c, 0x0e, 0x6e, 0xc7, 0x1f, 0xab, 0x58, 0x3b, 0x08, 0xbd, 0x81, 0x37,
  0x3c, 0x25, 0x5c, 0x3c, 0x37, 0x1b, 0x2e, 0x84, 0x86, 0x3c, 0x98, 0xa4, 0xf1,
  0xe0, 0x8b, 0x74, 0x23, 0x5d, 0x14, 0xfb, 0x5d, 0x9c, 0x0c, 0xd5, 0x46, 0xd9,
  0x68, 0x5f, 0x91, 0x3a, 0x0c, 0x0b, 0x2c, 0xc5, 0x34, 0x15, 0x83, 0xbf, 0x4b,
  0x43, 0x92, 0xe4, 0x67, 0xdb, 0x96, 0xd6, 0x5b, 0x9b, 0xb4, 0xcb, 0x71, 0x71,
  0x12, 0xf8, 0x47, 0x2e, 0x0d, 0x5a, 0x4d, 0x14, 0x50, 0x5f, 0xfd, 0x74, 0x84,
  0xb0, 0x12, 0x91, 0x09, 0x1c, 0x5f, 0x87, 0xb9, 0x88, 0x83, 0x46, 0x3f, 0x98,
  0x09, 0x1a, 0x0b, 0xaa, 0xae,
]);

export function verifyChallenge(data: VerifyData): string | undefined {
  try {
    const delegationChain = {
      delegations: data.delegationIdentity.delegations,
      publicKey: data.delegationIdentity.userPublicKey,
    };
    const currentTimeNanoSeconds = process.hrtime.bigint();
    const res = validateDelegationAndGetPrincipal(
      Uint8Array.from(Buffer.from(data.challenge, "base64")),
      JSON.stringify(delegationChain),
      currentTimeNanoSeconds,
      "jqajs-xiaaa-aaaad-aab5q-cai",
      ROOT_PUBLIC_KEY_RAW
    );

    return res;
  } catch (e) {
    console.error(e);
    return undefined;
  }
}

const handleChallenge = async (req: IncomingMessage, res: ServerResponse) => {
  const challenge = await createChallenge();
  res.statusCode = 200;
  res.end(JSON.stringify({ challenge }));
};

const handleVerify = async (req: RequestWithBody, res: ServerResponse) => {
  res.statusCode = 200;
  const principal = verifyChallenge(req.body);
  console.log("principal", principal);
  // TODO: add cookie here on success
  res.end(JSON.stringify({ status: "ok" }));
};

// Extend IncomingMessage to include body
interface RequestWithBody extends IncomingMessage {
  body?: any;
}

const bodyParserPlugin = (): Plugin => ({
  name: "body-parser",
  configureServer(server) {
    server.middlewares.use(
      async (req: RequestWithBody, res: ServerResponse, next) => {
        // Only parse JSON bodies and only for POST requests
        if (
          req.method === "POST" &&
          req.headers["content-type"] === "application/json"
        ) {
          let body = "";
          req.on("data", (chunk) => {
            body += chunk.toString(); // convert Buffer to string
          });
          req.on("end", () => {
            try {
              req.body = JSON.parse(body);
            } catch (e: unknown) {
              res.statusCode = 400;
              return res.end("Error parsing JSON body");
            }
            next();
          });
        } else {
          next();
        }
      }
    );
  },
});

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
  plugins: [bodyParserPlugin(), backendPlugin()],
  server: { port: 5178 },
});
