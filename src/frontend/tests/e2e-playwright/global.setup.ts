import { test as setup } from "@playwright/test";

setup("global setup", async ({ page }) => {
  await page.addInitScript(() => {
    // We override the function in the browser's context
    // This script will run every time a page is created or navigated to
    // We use Math.random() to determine the A/B test group
    // In E2E runs, we force the A/B test group to be the same for all tests
    Object.defineProperty(Math, "random", {
      value: () => 1,
    });
  });
});
