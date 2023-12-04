import { isNullish } from "@dfinity/utils";
import { minify } from "html-minifier-terser";
import httpProxy from "http-proxy";
import { extname } from "path";
import { Plugin, ViteDevServer } from "vite";
import viteCompression from "vite-plugin-compression";
import { readCanisterId } from "./utils";

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
    const canisterId = readCanisterId({
      canisterName: "internet_identity",
    });

    return html.replace(rgx, (_match, src) => {
      return `<script data-canister-id="${canisterId}" type="module" src="${src}"></script>`;
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
export const replicaForwardPlugin = ({
  replicaOrigin,
  forwardDomains /* note: will match exactly on <canister>.<domain> */,
  forwardRules,
}: {
  replicaOrigin: string;
  forwardDomains?: string[];
  forwardRules: Array<{ canisterName: string; hosts: string[] }>;
}) => ({
  name: "replica-forward",
  configureServer(server: ViteDevServer) {
    const proxy = httpProxy.createProxyServer({
      secure: false,
    });

    server.middlewares.use((req, res, next) => {
      if (
        /* Deny requests to raw URLs, e.g. <canisterId>.raw.ic0.app to make sure that II always uses certified assets
         * to verify the alternative origins. */
        req.headers["host"]?.includes(".raw.")
      ) {
        console.log(
          `Denying access to raw URL ${req.method} https://${req.headers.host}${req.url}`
        );
        res.statusCode = 400;
        res.end("Raw IC URLs are not supported");
        return;
      }

      const host = req.headers["host"];
      if (isNullish(host)) {
        // default handling
        return next();
      }

      // forward to the specified canister (served by the replica)
      const forwardToReplica = ({ canisterId }: { canisterId: string }) => {
        console.log(
          `forwarding ${req.method} https://${req.headers.host}${req.url} to canister ${canisterId}`
        );
        req.headers["host"] = `${canisterId}.localhost`;
        proxy.web(req, res, {
          target: `http://${replicaOrigin}`,
        });

        proxy.on("error", (err: Error) => {
          res.statusCode = 500;
          res.end("Replica forwarding failed: " + err.message);
        });

        /* Add a 'x-ic-canister-id' header like the BNs do */
        proxy.on("proxyRes", (res) => {
          res.headers["x-ic-canister-id"] = canisterId;

          // Ensure the browser accepts the response
          res.headers["access-control-allow-origin"] = "*";
          res.headers["access-control-expose-headers"] = "*";
          res.headers["access-control-allow-headers"] = "*";
        });
      };

      const matchingRule = forwardRules.find((rule) =>
        rule.hosts.includes(host)
      );

      if (!isNullish(matchingRule)) {
        const canisterId = readCanisterId({
          canisterName: matchingRule.canisterName,
        });
        return forwardToReplica({ canisterId });
      }

      // split the subdomain & domain by splitting on the first dot
      const [subdomain, ...domain_] = host.split(".");
      const domain = domain_.join(".");

      if (
        !isNullish(forwardDomains) &&
        forwardDomains.includes(domain) &&
        /([a-z0-9])+(-[a-z0-9]+)+/.test(
          subdomain
        ) /* fast check for principal-ish */
      ) {
        // Assume the principal-ish thing is a canister ID
        return forwardToReplica({ canisterId: subdomain });
      }

      return next();
    });
  },
});
