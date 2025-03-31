import { browser } from "@wdio/globals";

describe("authentication", () => {
  it("works without webauthn", async () => {
    await browser.url("/");

    await browser.$("h1").waitForExist();

    // Click on "Create an Internet Identity Anchor"
    await browser.$("#registerButton").click();

    await browser.$("h1").waitForExist();
    const title = await browser.$("h1");

    await browser.waitUntil(
      async () => {
        return (
          (await title.getText()) === "You’ve created an Internet Identity!"
        );
      },
      { timeout: 20_000 },
    );
  });
});
