import { render } from "@lit-labs/ssr/lib/render-lit-html";
import { readFileSync } from "fs";
import { PluginOption } from "vite";
import { pageContent as aboutStaticContent } from "./src/frontend/src/flows/about";
import { I18n } from "./src/frontend/src/i18n";

/**
 * Read the II canister ID from dfx's local state
 */
const readCanisterId = (): string => {
  const canisterIdsJsonFile = "./.dfx/local/canister_ids.json";

  try {
    const {
      internet_identity: { local: canisterId },
    } = JSON.parse(readFileSync(canisterIdsJsonFile, "utf-8"));

    const assertNonNullish: (
      value: string
    ) => asserts value is NonNullable<string> = (value: string): void => {
      if (value === null || value === undefined) {
        throw new Error("Internet identity canister ID undefined");
      }
    };

    assertNonNullish(canisterId);

    console.log("Read canister ID:", canisterId);

    return canisterId;
  } catch (e) {
    throw Error(`Could not get canister ID from ${canisterIdsJsonFile}: ${e}`);
  }
};

/**
 * Inject the II canister ID as a <script /> tag in index.html for local development.
 */
export const injectCanisterIdPlugin = (): {
  name: "html-transform";
  transformIndexHtml(html: string): string;
} => ({
  name: "html-transform",
  transformIndexHtml(html): string {
    return html.replace(
      `<script id="setupJs"></script>`,
      `<script data-canister-id="${readCanisterId()}" id="setupJs"></script>`
    );
  },
});

/**
 * Remove the <script /> that loads the index.js for production build.
 * A script loader is injected by the backend.
 */
export const stripInjectJsScript = (): {
  name: "html-transform";
  transformIndexHtml(html: string): string;
} => ({
  name: "html-transform",
  transformIndexHtml(html): string {
    return html.replace(
      `<script type="module" crossorigin src="/index.js"></script>`,
      ``
    );
  },
});

/**
 * Pre-render about.html
 *
 * 1. Plugin transform only /about.html
 * 2. When such file is detected, it uses lit SSR render function to render the TypeScript file to a string
 * 3. It finally replaces the JavaScript content in the HTML content as if it would be loaded at runtime with the static content that was evaluated
 */
export const preRenderAboutPlugin = (): PluginOption => ({
  name: "html-transform",
  transformIndexHtml(html: string, { path }: { path: string }): string {
    if (path !== "/about.html") {
      return html;
    }

    const pageContent = aboutStaticContent(new I18n());
    const content = Array.from(render(pageContent)).reduce(
        (acc, v) => acc + v,
        ""
    );

    return html.replace(
      '<main id="pageContent" class="l-wrap" aria-live="polite"></main>',
      `<main id="pageContent" class="l-wrap" aria-live="polite">${content}</main>`
    );
  },
});
