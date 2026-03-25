import { chromium, type BrowserContext } from "@playwright/test";
import path from "path";
import { test } from "./";

const pathToExtension = path.resolve("demos/test-app/dist");

export const chromeExtensionTest = test.extend<{
  context: BrowserContext;
  extensionId: string;
  extensionUrl: string;
}>({
  // Spread operator is required by PlayWright
  // eslint-disable-next-line no-empty-pattern
  context: async ({}, use) => {
    const context = await chromium.launchPersistentContext("", {
      channel: "chromium",
      args: [
        `--disable-extensions-except=${pathToExtension}`,
        `--load-extension=${pathToExtension}`,
        "--ignore-certificate-errors",
        "--host-resolver-rules=MAP * localhost:5173, EXCLUDE localhost",
      ],
    });
    // Close the default about:blank page so that page indices align
    // with what the authorize fixtures expect.
    const [defaultPage] = context.pages();
    if (defaultPage !== undefined) {
      await defaultPage.close();
    }
    await use(context);
    await context.close();
  },
  extensionId: async ({ context }, use) => {
    let [serviceWorker] = context.serviceWorkers();
    if (serviceWorker === undefined) {
      serviceWorker = await context.waitForEvent("serviceworker");
    }
    await use(serviceWorker.url().split("/")[2]);
  },
  extensionUrl: async ({ extensionId }, use) => {
    await use(`chrome-extension://${extensionId}/index.html`);
  },
});
