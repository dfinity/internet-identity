import { test as base, expect } from "@playwright/test";

/**
 * Custom test fixture that automatically applies host resolution routing
 * to redirect all non-localhost requests to localhost:5173
 */
export const test = base.extend({
  page: async ({ page }, use) => {
    // Safari doesn't support --host-resolver-rules, so we need page routing for Safari
    // Chromium browsers will use host-resolver-rules which preserves Host headers better
    const browserName = page.context().browser()?.browserType().name();

    if (browserName === "webkit") {
      // Apply routing for Safari since it doesn't support host-resolver-rules
      await page.route("**/*", (route) => {
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

        // The vite server uses
        const newUrl = `https://internet_identity.localhost:5173${url.pathname}${url.search}`;
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
