import { runInBrowser } from "../../util";

import { WebDriverPageObjectElement } from "../../webDriverPageObjectElement";
import { authorize, createPasskeyIdentity } from "./utils";
import { expect } from "vitest";

test("Authorize with last used account", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    const { credential, principal } = await createPasskeyIdentity({ browser });
    const { principal: expectedPrincipal } = await authorize(
      async () => {
        const page = WebDriverPageObjectElement.create(browser);
        await page.getByRole("button", { name: "Continue" }).click();
      },
      { browser, credential },
    );
    expect(principal).toBe(expectedPrincipal);
  });
}, 300_000);
