describe("authentication", () => {
  it("works without webauthn", async () => {
    await browser.url("/");

    await browser.$("h1").waitForExist();

    // Click on "Create an Internet Identity Anchor"
    await browser.$("#registerButton").click();

    // Set the name of the device and submit
    const registerAlias = await browser.$("#pickAliasInput");
    await registerAlias.waitForExist();
    await registerAlias.setValue("My Device");

    await browser.$('button[type="submit"]').click();

    // Construct Identity (no-op)
    const constructIdentity = await browser.$(
      '[data-action="construct-identity"]'
    );
    await constructIdentity.waitForExist();
    await constructIdentity.click();

    // Pass Captcha
    const captchaInput = await browser.$("#captchaInput");
    await captchaInput.waitForExist();
    await captchaInput.setValue("a");
    await browser.waitUntil(async () => {
      return (await captchaInput.getValue()) === "a";
    });

    const registerButton = await browser.$("#confirmRegisterButton");
    await registerButton.waitForEnabled({ timeout: 30_000 });
    await registerButton.click();

    await browser.$("h1").waitForExist();
    const title = await browser.$("h1");

    await browser.waitUntil(
      async () => {
        return (
          (await title.getText()) ===
          "You’ve created an Internet Identity!"
        );
      },
      { timeout: 20_000 }
    );
  });
});
