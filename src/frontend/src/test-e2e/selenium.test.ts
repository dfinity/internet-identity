import {
  Builder,
  By,
  until,
  ThenableWebDriver,
  logging,
} from "selenium-webdriver";
import { Command } from "selenium-webdriver/lib/command";
import { Options as ChromeOptions } from "selenium-webdriver/chrome";
import { writeFile } from "fs/promises";

/*
## Structure of this file

 - Constants

 - Setup helpers (getting web driver, waiting for fonts to be loaded)

 - Combined flows
   Sequences of assertions and actions used in multiple tests to model
   higher-level concept

 - The actual tests
*/

/*
## Constants
*/

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import canister_ids1 from "../../../../.dfx/local/canister_ids.json";

const IDENTITY_CANISTER = canister_ids1.internet_identity.local;
import canister_ids2 from "../../../../demos/whoami/.dfx/local/canister_ids.json";
import {
  AboutView,
  AddDeviceAliasView, AddDeviceView, AddIdentityAnchorView,
  AuthorizeAppView, CompatabilityNoticeView, DemoAppView,
  FLOWS,
  MainView, RecoveryMethodSelector,
  RegisterView, SingleDeviceWarningView,
  WelcomeBackView,
  WelcomeView
} from "./views";

const WHOAMI_CANISTER = canister_ids2.whoami.local;

const REPLICA_URL = "http://localhost:8000";
const II_URL = `http://localhost:8000/?canisterId=${IDENTITY_CANISTER}`;
const DEMO_APP_URL = "http://localhost:8080/";

const DEVICE_NAME1 = "Virtual WebAuthn device";
const DEVICE_NAME2 = "Other WebAuthn device";

/*
## Setup helpers
*/

async function addVirtualAuthenticator(
  driver: ThenableWebDriver
): Promise<string> {
  const executor = driver.getExecutor();
  const sessionId = (await driver.getSession()).getId();
  executor.defineCommand(
    "AddVirtualAuthenticator",
    "POST",
    "/session/:sessionId/webauthn/authenticator"
  );
  const cmd = new Command("AddVirtualAuthenticator");
  cmd.setParameter("protocol", "ctap2");
  cmd.setParameter("transport", "usb");
  cmd.setParameter("hasResidentKey", true);
  cmd.setParameter("isUserConsenting", true);
  cmd.setParameter("sessionId", sessionId);
  return await executor.execute(cmd);
}

async function removeVirtualAuthenticator(
  driver: ThenableWebDriver,
  authenticatorId: string
): Promise<string> {
  const executor = driver.getExecutor();
  const sessionId = (await driver.getSession()).getId();
  executor.defineCommand(
    "RemoveVirtualAuthenticator",
    "DELETE",
    `/session/:sessionId/webauthn/authenticator/${authenticatorId}`
  );
  const cmd = new Command("RemoveVirtualAuthenticator");
  cmd.setParameter("sessionId", sessionId);
  return await executor.execute(cmd);
}

async function screenshot(name: string, driver: ThenableWebDriver) {
  const image = await driver.takeScreenshot();
  // writing to a subdirectory has the nice property that it fails if
  // this is run in the wrong directory
  await writeFile(`screenshots/${name}.png`, image, "base64");
}

// Inspired by https://stackoverflow.com/a/66919695/946226
async function wait_for_fonts(driver: ThenableWebDriver) {
  for (let i = 0; i <= 50; i++) {
    if (
      (await driver.executeScript("return document.fonts.status;")) == "loaded"
    ) {
      return;
    }
    driver.sleep(200);
  }
  console.log(
    "Odd, document.font.status never reached state loaded, stuck at",
    await driver.executeScript("return document.fonts.status;")
  );
}

async function run_in_browser(
  test: (driver: ThenableWebDriver) => Promise<void>
) {
  await run_in_browser_common(true, test);
}

async function run_in_nested_browser(
  test: (driver: ThenableWebDriver) => Promise<void>
) {
  await run_in_browser_common(false, test);
}

async function run_in_browser_common(
  outer: boolean,
  test: (driver: ThenableWebDriver) => Promise<void>
) {
  const loggingPreferences = new logging.Preferences();
  loggingPreferences.setLevel("browser", logging.Level.ALL);
  const driver = new Builder()
    .forBrowser("chrome")
    .setChromeOptions(
      new ChromeOptions()
        .headless() // hides the click show: uncomment to watch it
        .windowSize({width: 1050, height: 1400})
    )
    .setLoggingPrefs(loggingPreferences)
    .build();
  try {
    await test(driver);
  } catch (e) {
    console.log(await driver.manage().logs().get("browser"));
    console.log(await driver.getPageSource());
    throw e;
  } finally {
    // only close outer session
    if (outer) {
      await driver.quit();
    }
  }
}

async function switchToPopup(driver: ThenableWebDriver) {
  let handles = await driver.getAllWindowHandles();
  expect(handles.length).toBe(2);
  await driver.switchTo().window(handles[1]);
  // enable virtual authenticator in the new window
  await addVirtualAuthenticator(driver);
}

async function waitToClose(driver: ThenableWebDriver) {
  await driver.wait(async (driver: ThenableWebDriver) => {
    return (await driver.getAllWindowHandles()).length == 1;
  }, 10_000);

  const handles = await driver.getAllWindowHandles();
  expect(handles.length).toBe(1);
  await driver.switchTo().window(handles[0]);
}

/*
## The actual tests
*/
test("_Register new identity and login with it", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    await driver.get(II_URL);
    const userNumber = await FLOWS.registerNewIdentity(DEVICE_NAME1, driver);
    const mainView = new MainView(driver);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.logout();
    await FLOWS.login(userNumber, DEVICE_NAME1, driver);
  });
}, 300_000);

test("Register new identity and add additional device", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    const mainView = new MainView(driver);
    const addDeviceAliasView = new AddDeviceAliasView(driver);

    const firstAuthenticator = await addVirtualAuthenticator(driver);
    await driver.get(II_URL);
    const userNumber = await FLOWS.registerNewIdentity(DEVICE_NAME1, driver);

    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    // We're removing the first authenticator here, because unfortunately we
    // can't tell Chrome to _actually_ use the second authenticator, which
    // leads to flaky tests otherwise.
    await removeVirtualAuthenticator(driver, firstAuthenticator);
    await addVirtualAuthenticator(driver);
    await mainView.addAdditionalDevice();

    await addDeviceAliasView.waitForDisplay();
    await addDeviceAliasView.addAdditionalDevice(DEVICE_NAME2);
    await addDeviceAliasView.continue();

    await driver.sleep(10_000);

    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.waitForDeviceDisplay(DEVICE_NAME2);

    await mainView.logout();
    await FLOWS.login(userNumber, DEVICE_NAME1, driver);
  });
}, 300_000);

test("Log into client application, after registration", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    const demoAppView = new DemoAppView(driver);
    await addVirtualAuthenticator(driver);
    await demoAppView.open(DEMO_APP_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.signin();
    await switchToPopup(driver);
    await FLOWS.registerNewIdentity(DEVICE_NAME1, driver);
    const authorizeAppView = new AuthorizeAppView(driver);
    await authorizeAppView.waitForDisplay();
    await authorizeAppView.confirm();
    await waitToClose(driver);
    await demoAppView.waitForDisplay();
    const principal = await demoAppView.getPrincipal();
    expect(principal).not.toBe("2vxsx-fae");
    expect(await demoAppView.whoami(REPLICA_URL, WHOAMI_CANISTER)).toBe(principal);
    // default value
    const exp = await driver.findElement(By.id("expiration")).getText();
    expect(Number(exp) / (30 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Delegation maxTimeToLive: 1 min", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    const demoAppView = new DemoAppView(driver);
    await addVirtualAuthenticator(driver);
    await demoAppView.open(DEMO_APP_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.setMaxTimeToLive(BigInt(60_000_000_000));
    await demoAppView.signin();
    await switchToPopup(driver);
    await FLOWS.registerNewIdentity(DEVICE_NAME1, driver);
    const authorizeAppView = new AuthorizeAppView(driver);
    await authorizeAppView.waitForDisplay();
    await authorizeAppView.confirm();
    await waitToClose(driver);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).not.toBe("2vxsx-fae");
    const exp = await driver.findElement(By.id("expiration")).getText();
    // compare only up to one decimal place for the 1min test
    expect(Number(exp) / 60_000_000_000).toBeCloseTo(1, 0);
  });
}, 300_000);

test("Delegation maxTimeToLive: 1 day", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    const demoAppView = new DemoAppView(driver);
    await addVirtualAuthenticator(driver);
    await demoAppView.open(DEMO_APP_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.setMaxTimeToLive(BigInt(86400_000_000_000));
    await demoAppView.signin();
    await switchToPopup(driver);
    await FLOWS.registerNewIdentity(DEVICE_NAME1, driver);
    const authorizeAppView = new AuthorizeAppView(driver);
    await authorizeAppView.waitForDisplay();
    await authorizeAppView.confirm();
    await waitToClose(driver);
    expect(await demoAppView.getPrincipal()).not.toBe("2vxsx-fae");
    const exp = await driver.findElement(By.id("expiration")).getText();
    expect(Number(exp) / 86400_000_000_000).toBeCloseTo(1);
  });
}, 300_000);

test("Delegation maxTimeToLive: 1 month", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    const demoAppView = new DemoAppView(driver);
    await addVirtualAuthenticator(driver);
    await demoAppView.open(DEMO_APP_URL, II_URL);
    await demoAppView.waitForDisplay();
    expect(await demoAppView.getPrincipal()).toBe("2vxsx-fae");
    await demoAppView.setMaxTimeToLive(BigInt(2592000_000_000_000));
    await demoAppView.signin();
    await switchToPopup(driver);
    await FLOWS.registerNewIdentity(DEVICE_NAME1, driver);
    const authorizeAppView = new AuthorizeAppView(driver);
    await authorizeAppView.waitForDisplay();
    await authorizeAppView.confirm();
    await waitToClose(driver);
    expect(await demoAppView.getPrincipal()).not.toBe("2vxsx-fae");
    const exp = await driver.findElement(By.id("expiration")).getText();
    // NB: Max out at 8 days
    expect(Number(exp) / 691200_000_000_000).toBeCloseTo(1);
  });
}, 300_000);

test("Screenshots", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    const welcomeView = new WelcomeView(driver);
    const registerView = new RegisterView(driver);
    const recoveryMethodSelector = new RecoveryMethodSelector(driver);
    const singleDeviceWarningView = new SingleDeviceWarningView(driver);
    const mainView = new MainView(driver);
    const addDeviceView = new AddDeviceView(driver);
    const aboutView = new AboutView(driver);
    const compatabilityNoticeView = new CompatabilityNoticeView(driver);

    await addVirtualAuthenticator(driver);
    await driver.get(II_URL);

    await wait_for_fonts(driver);
    await welcomeView.waitForDisplay();
    await screenshot("00-welcome", driver);
    await welcomeView.register();
    await registerView.waitForDisplay();
    await screenshot("01-register", driver);
    await registerView.enterAlias(DEVICE_NAME1);
    await registerView.create();
    await registerView.waitForRegisterConfirm();
    await screenshot("02-register-confirm", driver);
    await registerView.confirmRegisterConfirm();
    await registerView.waitForIdentity();
    const userNumber = await registerView.registerGetIdentity();
    await registerView.registerIdentityFixup();
    await screenshot("03-register-user-number", driver);
    await registerView.registerConfirmIdentity();
    await singleDeviceWarningView.waitForDisplay();
    await screenshot("17-single-device-warning", driver);
    await singleDeviceWarningView.continue();
    await recoveryMethodSelector.waitForDisplay();
    await screenshot("18-recover-method-selector", driver);
    await recoveryMethodSelector.skip();
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.fixup();
    await screenshot("04-main", driver);
    await mainView.logout();
    await welcomeView.waitForDisplay(); // no point taking screenshot
    await welcomeView.typeUserNumber(userNumber);
    await welcomeView.login();
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.continue();
    await recoveryMethodSelector.waitForDisplay();
    await recoveryMethodSelector.skip();
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    await driver.get(II_URL);
    const welcomeBackView = new WelcomeBackView(driver);
    await welcomeBackView.waitForDisplay();
    const userNumber2 = await welcomeBackView.getIdentityAnchor();
    expect(userNumber2).toBe(userNumber);
    await welcomeBackView.fixup();
    await screenshot("05-welcome-back", driver);
    await welcomeBackView.login();
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.continue();
    await recoveryMethodSelector.waitForDisplay();
    await recoveryMethodSelector.skip();
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    // Now the link device flow, using a second browser
    await run_in_nested_browser(async (driver2) => {
      const welcomeView2 = new WelcomeView(driver2);
      const addIdentityAnchorView2 = new AddIdentityAnchorView(driver2);
      const recoveryMethodSelector2 = new RecoveryMethodSelector(driver2);
      const singleDeviceWarningView2 = new SingleDeviceWarningView(driver2);
      const mainView2 = new MainView(driver2);
      const addDeviceView2 = new AddDeviceView(driver2);

      await addVirtualAuthenticator(driver2);
      await driver2.get(II_URL);
      await welcomeView2.waitForDisplay();
      await welcomeView2.typeUserNumber(userNumber);
      await welcomeView2.addDevice();
      await addIdentityAnchorView2.waitForDisplay();
      await addIdentityAnchorView2.fixup();
      await screenshot("06-new-device-user-number", driver2);
      await addIdentityAnchorView2.continue(userNumber);
      await addDeviceView2.waitForDisplay();

      const link = await addDeviceView2.getLinkText();
      console.log("The add device link is", link);
      await addDeviceView2.fixup();
      await screenshot("07-new-device", driver2);

      // Log in with previous browser again
      await driver.get("about:blank");
      await driver.get(link);
      await wait_for_fonts(driver);
      const welcomeBackView = new WelcomeBackView(driver);
      await welcomeBackView.waitForDisplay();
      await welcomeBackView.fixup();
      await screenshot("08-new-device-login", driver);
      await welcomeBackView.login();
      await singleDeviceWarningView.waitForDisplay();
      await singleDeviceWarningView.continue();
      await recoveryMethodSelector.waitForDisplay();
      await recoveryMethodSelector.skip();
      await addDeviceView.waitForConfirmDisplay();
      await addDeviceView.fixupConfirm();
      await screenshot("09-new-device-confirm", driver);
      await addDeviceView.confirm();
      await addDeviceView.waitForAliasDisplay();
      await screenshot("10-new-device-alias", driver);
      await addDeviceView.addDeviceAlias(DEVICE_NAME2);
      await addDeviceView.addDeviceAliasContinue();
      await addDeviceView.waitForAddDeviceSuccess();
      await screenshot("11-new-device-done", driver);

      // Back to other browser, should be a welcome view now
      const welcomeBackView2 = new WelcomeBackView(driver2);
      await welcomeBackView2.waitForDisplay();
      await welcomeBackView2.fixup();
      await screenshot("12-new-device-login", driver2);
      await welcomeBackView2.login();
      await singleDeviceWarningView2.waitForDisplay();
      await singleDeviceWarningView2.continue();
      await recoveryMethodSelector2.waitForDisplay();
      await recoveryMethodSelector2.skip();
      await mainView2.waitForDeviceDisplay(DEVICE_NAME2);
      await mainView2.fixup();
      await screenshot("13-new-device-listed", driver2);

      // Try to remove current device
      await mainView2.removeDevice(DEVICE_NAME2);
      await driver2.wait(until.alertIsPresent(), 1_000);
      const alert = driver2.switchTo().alert();
      // Cannot take screenshots of modal dialogs, it seems
      // Also dismiss before asserting things, the exception
      // handler doesn’t work well while modal dialogs are open
      const alertText = await alert.getText();
      await alert.dismiss();
      expect(alertText).toBe(
        "This will remove your current device and you will be logged out"
      );
    });

    // About page
    await driver.get("about:blank");
    await driver.get(II_URL + "#about");
    await wait_for_fonts(driver);
    await aboutView.waitForDisplay();
    await screenshot("14-about", driver);

    // Test device removal
    await driver.get(II_URL);
    await welcomeBackView.waitForDisplay();
    const userNumber3 = await welcomeBackView.getIdentityAnchor();
    expect(userNumber3).toBe(userNumber);
    await welcomeBackView.login();
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.continue();
    await recoveryMethodSelector.waitForDisplay();
    await recoveryMethodSelector.skip();
    await mainView.waitForDeviceDisplay(DEVICE_NAME2);
    const buttonElem2 = await driver.findElement(
      By.xpath(`//div[string()='${DEVICE_NAME2}']/following-sibling::button`)
    );
    await mainView.removeDevice(DEVICE_NAME2);
    // No dialog here!

    await driver.wait(until.stalenessOf(buttonElem2));
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.fixup();
    await screenshot("15-after-removal", driver);

    await mainView.removeDevice(DEVICE_NAME1);
    const alert1 = driver.switchTo().alert();
    const alertText1 = await alert1.getText();
    await alert1.accept();
    expect(alertText1).toBe(
      "This will remove your current device and you will be logged out"
    );
    await driver.wait(until.alertIsPresent());
    const alert2 = driver.switchTo().alert();
    const alertText2 = await alert2.getText();
    await alert2.accept();
    expect(alertText2).toBe("You can not remove your last device.");
    // device still present. You can't remove your last device.
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    // Compatibility notice page
    await driver.get("about:blank");
    await driver.get(II_URL + "#compatibilityNotice");
    await wait_for_fonts(driver);
    await compatabilityNoticeView.waitForDisplay();
    await screenshot("16-compatibility-notice", driver);
  });
}, 400_000);
