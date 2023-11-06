import { assertNonNullish, isNullish } from "@dfinity/utils";
import { readFileSync } from "fs";
import { minify } from "html-minifier-terser";
import httpProxy from "http-proxy";
import { extname } from "path";
import { Plugin, ViteDevServer } from "vite";
import viteCompression from "vite-plugin-compression";

/**
 * Read a canister ID from dfx's local state
 */
export const readCanisterId = ({
  canisterName,
  canisterIdsJsonFile,
}: {
  canisterName: string;
  canisterIdsJsonFile: string;
}): string => {
  try {
    const canisterIds: Record<string, { local: string }> = JSON.parse(
      readFileSync(canisterIdsJsonFile, "utf-8")
    );
    const canisterId = canisterIds[canisterName]?.local;
    assertNonNullish(
      canisterId,
      `Could not get canister ID from ${canisterIdsJsonFile}`
    );
    console.log(
      `Read canister ID '${canisterId} for canister with name '${canisterName}'`
    );

    return canisterId;
  } catch (e) {
    throw Error(`Could not get canister ID from ${canisterIdsJsonFile}: ${e}`);
  }
};

/**
 * Inject the II canister ID as a <script /> tag in index.html for local development. Will process
 * at most 1 script tag.
 */
export const injectCanisterIdPlugin = (): {
  name: "html-transform";
  transformIndexHtml(html: string): string;
} => ({
  name: "html-transform",
  transformIndexHtml(html): string {
    const rgx = /<script type="module" src="(?<src>[^"]+)"><\/script>/;

    return html.replace(rgx, (_match, src) => {
      return `<script data-canister-id="${readCanisterId({
        canisterName: "internet_identity",
        canisterIdsJsonFile: "./.dfx/local/canister_ids.json",
      })}" type="module" src="${src}"></script>`;
    });
  },
});

/**
 * GZip generated resources e.g. index.js => index.js.gz
 */
export const compression = (): Plugin =>
  viteCompression({
    // II canister only supports one content type per resource. That is why we remove the original file.
    deleteOriginFile: true,
    filter: (file: string): boolean =>
      ![".html", ".css", ".webp", ".png", ".ico", ".svg"].includes(
        extname(file)
      ),
  });

/**
 * Minify HTML
 */
export const minifyHTML = (): {
  name: "html-transform";
  transformIndexHtml(html: string): Promise<string>;
} => ({
  name: "html-transform",
  async transformIndexHtml(html): Promise<string> {
    return minify(html, { collapseWhitespace: true });
  },
});

/**
 * Forwards requests to the local replica.
 * Denies access to raw URLs.
 *
 * @param replicaOrigin Replica URL to forward requests to
 * @param forwardRules List of rules (i.e. hostname to canisterId mappings)
 *                     to forward requests to a specific canister
 */
export const replicaForwardPlugin = (
  replicaOrigin: string,
  forwardRules: Array<{ canisterId: string; hosts: string[] }>
) => ({
  name: "replica-forward",
  configureServer(server: ViteDevServer) {
    const proxy = httpProxy.createProxyServer({
      secure: false,
    });

    server.middlewares.use((req, res, next) => {
      if (
        /* deny requests to raw URLs */
        req.headers["host"]?.endsWith(".raw.ic0.app") ||
        req.headers["host"]?.endsWith(".raw.icp0.io")
      ) {
        console.log(
          `Denying access to raw URL ${req.method} https://${req.headers.host}${req.url}`
        );
        res.statusCode = 400;
        res.end("Raw IC URLs are not supported");
        return;
      }

      const matchingRule = forwardRules.find((rule) => {
        let host = req.headers["host"];
        return isNullish(host) || rule.hosts.includes(host);
      });
      if (isNullish(matchingRule)) {
        // default handling
        return next();
      }

      console.log(
        `forwarding ${req.method} https://${req.headers.host}${req.url} to canister ${matchingRule.canisterId}`
      );
      req.headers["host"] = `${matchingRule.canisterId}.localhost`;
      proxy.web(req, res, {
        target: `http://${replicaOrigin}`,
      });

      proxy.on("error", (err: Error) => {
        res.statusCode = 500;
        res.end("Replica forwarding failed: " + err.message);
      });
    });
  },
});
