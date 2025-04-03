import { isNullish, nonNullish } from "@dfinity/utils";
import { minify } from "html-minifier-terser";
import type { Plugin, ViteDevServer } from "vite";
import viteCompression from "vite-plugin-compression2";
import {
  forwardToReplica,
  readCanisterConfig,
  readCanisterId,
  readReplicaPort,
} from "./utils.js";

export * from "./utils.js";

/**
 * Inject the canister ID of 'canisterName' as a <script /> tag in index.html for local development. Will process
 * at most 1 script tag.
 */
export const injectCanisterIdPlugin = ({
  canisterName,
}: {
  canisterName: string;
}): Plugin => ({
  name: "inject-canister-id",
  transformIndexHtml(html): string {
    const rgx = /<script type="module" src="(?<src>[^"]+)"><\/script>/;
    const canisterId = readCanisterId({ canisterName });
    return html.replace(rgx, (_match, src) => {
      return `<script data-canister-id="${canisterId}" type="module" src="${src}"></script>`;
    });
  },
});

/**
 * Inject the canister ID and config of 'canisterName' as a <script /> tag in index.html for local development. Will process
 * at most 1 script tag.
 */
export const injectCanisterIdAndConfigPlugin = ({
  canisterName,
}: {
  canisterName: string;
}): Plugin => ({
  name: "inject-canister-id-and-config",
  transformIndexHtml(html): string {
    const rgx = /<body /;
    const canisterId = readCanisterId({ canisterName });
    const canisterConfig = readCanisterConfig({ canisterName });
    return html.replace(rgx, (_match, src) => {
      return `<body data-canister-id="${canisterId}" data-canister-config="${canisterConfig}" `;
    });
  },
});

/**
 * GZip generated resources e.g. index.js => index.js.gz
 */
export const compression = (): Plugin =>
  viteCompression({
    // II canister only supports one content type per resource. That is why we remove the original file.
    algorithm: "gzip",
    deleteOriginalAssets: true,
    include: /\.(js|woff2)$/,
  });

/**
 * Minify HTML
 */
export const minifyHTML = (): Plugin => ({
  name: "minify-html",
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
  forwardDomains /* note: will match exactly on <canister>.<domain> */,
  forwardRules,
}: {
  forwardDomains?: string[];
  forwardRules: Array<{ canisterName: string; hosts: string[] }>;
}) => ({
  name: "replica-forward",
  configureServer(server: ViteDevServer) {
    const replicaOrigin = `127.0.0.1:${readReplicaPort()}`;
    server.middlewares.use((req, res, next) => {
      if (
        /* Deny requests to raw URLs, e.g. <canisterId>.raw.ic0.app to make sure that II always uses certified assets
         * to verify the alternative origins. */
        req.headers["host"]?.includes(".raw.")
      ) {
        console.log(
          `Denying access to raw URL ${req.method} https://${req.headers.host}${req.url}`,
        );
        res.statusCode = 400;
        res.end("Raw IC URLs are not supported");
        return;
      }

      const host_ = req.headers["host"];
      if (isNullish(host_)) {
        // default handling
        return next();
      }

      const [host, _port] = host_.split(":");

      const matchingRule = forwardRules.find((rule) =>
        rule.hosts.includes(host),
      );

      if (!isNullish(matchingRule)) {
        const canisterId = readCanisterId({
          canisterName: matchingRule.canisterName,
        });
        console.log("Host matches forward rule", host);
        return forwardToReplica({ canisterId, req, res, replicaOrigin });
      }

      // split the subdomain & domain by splitting on the first dot
      const [subdomain_, ...domain_] = host.split(".");
      const [subdomain, domain] =
        domain_.length > 0
          ? [subdomain_, domain_.join(".")]
          : [undefined, subdomain_];

      if (
        nonNullish(forwardDomains) &&
        nonNullish(subdomain) &&
        forwardDomains.includes(domain) &&
        /([a-z0-9])+(-[a-z0-9]+)+/.test(
          subdomain,
        ) /* fast check for principal-ish */
      ) {
        // Assume the principal-ish thing is a canister ID
        console.log("Domain matches list to forward", domain);
        return forwardToReplica({
          canisterId: subdomain,
          req,
          res,
          replicaOrigin,
        });
      }

      // Try to read the canister ID of a potential canister called <subdomain>
      // and if found forward to that
      if (nonNullish(subdomain) && domain === "localhost") {
        try {
          const canisterId = readCanisterId({ canisterName: subdomain });
          console.log("Subdomain is a canister", subdomain, canisterId);
          return forwardToReplica({ canisterId, req, res, replicaOrigin });
        } catch {}
      }

      return next();
    });
  },
});

/** Update the HTML files to inline script imports.
 * i.e.: `<script src="foo.js"></script>` becomes `<script>... insert new script node...</script>`.
 *
 * This allows us to use the `strict-dynamic` CSP directive.
 * Otherwise, the directive requires `integrity=...` attributes which (in Chrome) does not easily allow importing
 * other scripts. https://github.com/WICG/import-maps/issues/174#issuecomment-987678704
 */
export const inlineScriptsPlugin: Plugin = {
  name: "integrity",
  apply: "build" /* only use during build, not serve */,

  transformIndexHtml(html: string): string {
    const rgx =
      /<script(?<attrs>(?<beforesrc>\s+[^>]+)*\s+src="?(?<src>[^"]+)"?(?<aftersrc>\s+[^>]+)*)>/g;
    return html.replace(rgx, (match, _attrs, beforesrc, src, aftersrc) => {
      const tag = ["script", beforesrc, aftersrc].filter(Boolean).join(" ");
      return `<${tag}>let s = document.createElement('script');s.type = 'module';s.src = '${src}';document.head.appendChild(s);`;
    });
  },
};
