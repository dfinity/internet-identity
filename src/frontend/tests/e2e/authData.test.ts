import { FLOWS } from "./flows";
import { addVirtualAuthenticator, runInBrowser, switchToPopup } from "./util";
import { DemoAppView, ErrorView } from "./views";

import { II_URL, TEST_APP_NICE_URL } from "./constants";

test("Authorize ready message should be sent immediately", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();
    await demoAppView.waitForNthMessage(1);
    expect(await demoAppView.getMessageText(1)).toContain("authorize-ready");
  });
}, 300_000);

test("Should allow valid message", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();
    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);
    await demoAppView.waitForNthMessage(1); // message 1: authorize-ready
    await demoAppView.sendValidMessage(); // message 2: authorize-client

    await switchToPopup(browser);
    await FLOWS.registerNewIdentityAuthenticateView(browser);
    await browser
      .$("[data-role=notify-auth-success]")
      .waitForDisplayed({ timeout: 15_000 });

    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);

    await demoAppView.waitForNthMessage(3); // message 3: authorize-success
    const successMessage = await demoAppView.getMessageText(3);
    expect(successMessage).toContain("authorize-client-success");

    // Internet Identity default value (as opposed to agent-js)
    const exp = await browser.$("#expiration").getText();
    expect(Number(exp) / (30 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Should show error for invalid message", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();
    // switch back to demo app
    await browser.switchToWindow((await browser.getWindowHandles())[0]);
    await demoAppView.waitForNthMessage(1); // message 1: authorize-ready
    await demoAppView.sendInvalidData(); // message 2

    await switchToPopup(browser);
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      "It seems like an invalid authentication request was received.",
    );
  });
}, 300_000);

test("Should show error after not receiving message for 10 seconds", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await demoAppView.openIiTab();

    await switchToPopup(browser);
    await browser.pause(10_000);
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      "It seems like the connection with the service could not be established.",
    );
  });
}, 300_000);

test("Should show error after manually navigating to authorize url", async () => {
  await runInBrowser(async (browser: WebdriverIO.Browser) => {
    await addVirtualAuthenticator(browser);
    const demoAppView = new DemoAppView(browser);
    await demoAppView.open(TEST_APP_NICE_URL, II_URL);
    await demoAppView.waitForDisplay();
    await browser.url(II_URL + "#authorize");
    const errorView = new ErrorView(browser);
    await errorView.waitForDisplay();
    expect(await errorView.getErrorMessage()).toEqual(
      "There was an issue connecting with the application. Try a different browser; if the issue persists, contact the developer.",
    );
  });
}, 300_000);
