describe("landing page", () => {
  it("loads", async () => {
    await browser.url("/");

    await browser.$("h2").waitForExist();

    await browser["screenshot"]("landing-page");
  });
});
