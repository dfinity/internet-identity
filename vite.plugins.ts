import { assertNonNullish } from "@dfinity/utils";
import { readFileSync } from "fs";

/**
 * Read the II canister ID from dfx's local state
 */
const readCanisterId = (): string => {
  const canisterIdsJsonFile = "./.dfx/local/canister_ids.json";

  try {
    const {
      internet_identity: { local: canisterId },
    } = JSON.parse(readFileSync(canisterIdsJsonFile, "utf-8"));

    assertNonNullish(
      canisterId,
      `Could not get canister ID from ${canisterIdsJsonFile}`
    );

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
