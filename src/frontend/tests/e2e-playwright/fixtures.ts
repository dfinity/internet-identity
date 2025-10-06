import { test as base, expect } from "@playwright/test";

/**
 * Custom test fixture that automatically applies host resolution routing
 * to redirect all non-localhost requests to localhost:5173
 *
 * ⚠️ IMPORTANT: All E2E test files MUST import { test, expect } from this file
 * instead of from '@playwright/test' to ensure proper host routing for Safari/WebKit.
 *
 * @example
 * // ✅ Correct
 * import { test, expect } from "./fixtures";
 * // or from subdirectories:
 * import { test, expect } from "../fixtures";
 *
 * // ❌ Wrong - will fail ESLint
 * import { test, expect } from "@playwright/test";
 */
export const test = base.extend({
  page: async ({ page }, use) => {
    // Safari doesn't support --host-resolver-rules, so we need page routing for Safari
    // Chromium browsers will use host-resolver-rules which preserves Host headers better
    const browserName = page.context().browser()?.browserType().name();

    if (browserName === "webkit") {
      // Apply routing for Safari since it doesn't support host-resolver-rules
      await page.context().route("**/*", (route) => {
        // Should map the config in `vite.config.ts`
        const hostToCanisterName: Record<string, string> = {
          ["id.ai"]: "internet_identity",
          ["identity.ic0.app"]: "internet_identity",
          ["identity.internetcomputer.org"]: "internet_identity",
          ["nice-name.com"]: "test_app",
        };

        const req = route.request();
        const urlStr = req.url();

        let url: URL;
        try {
          url = new URL(urlStr);
        } catch {
          return route.continue();
        }

        if (url.hostname.includes("localhost")) {
          return route.continue();
        }

        const canister_name = hostToCanisterName[url.hostname];
        if (canister_name === undefined) {
          return route.continue();
        }
        // The vite server uses the Host header and the localhost subdomain to determine where the redirect the request.
        const newUrl = `https://${canister_name}.localhost:5173${url.pathname}${url.search}`;
        return route.continue({
          url: newUrl,
          // The vite server uses the Host header to determine where the redirect the request.
          headers: { ...req.headers(), Host: url.hostname },
        });
      });
    }

    // Use the page with routing applied
    await use(page);
  },
});

// Re-export expect for convenience
export { expect };
