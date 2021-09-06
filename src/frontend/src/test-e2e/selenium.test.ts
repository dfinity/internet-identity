import {
  Builder,
  By,
  until,
  ThenableWebDriver,
  Key,
  logging,
} from "selenium-webdriver";
import { Command } from "selenium-webdriver/lib/command";
import { Options as ChromeOptions } from "selenium-webdriver/chrome";
import { writeFile } from "fs/promises";

/*
## Structure of this file

 - Constants

 - Assertions and actions, grouped by View

   - on_X() assertions:
     These wait for a characteristic element of view `X`
     Some return useful data from view `X`

   - on_X_Y() actions:
     These should be run on_ `X`, and they perform the action `Y`
     Most actions will go to a different view, but not all of them.

   - on_X_Fixup() actions:
     These should be run on_ view `X`, and they replace variable data
     (mostly usernames) with fixed data for screenshots

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
const WHOAMI_CANISTER = canister_ids2.whoami.local;

const REPLICA_URL = "http://localhost:8000";
const II_URL = `http://localhost:8000/?canisterId=${IDENTITY_CANISTER}`;
const DEMO_APP_URL = "http://localhost:8080/";

const DEVICE_NAME1 = "Virtual WebAuthn device";
const DEVICE_NAME2 = "Other WebAuthn device";

/*
## Per-view helpers
*/

// View: Welcome

async function on_Welcome(driver: ThenableWebDriver) {
  await driver.wait(until.elementLocated(By.id("registerUserNumber")), 10_000);
}

async function on_Welcome_TypeUserNumber(
  user_number: string,
  driver: ThenableWebDriver
) {
  await driver.findElement(By.id("registerUserNumber")).sendKeys(user_number);
}

async function on_Welcome_Login(driver: ThenableWebDriver) {
  await driver.findElement(By.id("loginButton")).click();
}

async function on_Welcome_Register(driver: ThenableWebDriver) {
  await driver.findElement(By.id("registerButton")).click();
}

async function on_Welcome_AddDevice(driver: ThenableWebDriver) {
  await driver.findElement(By.id("addNewDeviceButton")).click();
}

// View: Register
// eslint-disable-next-line
async function on_Register(driver: ThenableWebDriver) {}

async function on_Register_TypeAliasEnter(
  alias: string,
  driver: ThenableWebDriver
) {
  await driver.findElement(By.id("registerAlias")).sendKeys(alias, Key.RETURN);
}

// View: Register confirmation

async function on_RegisterConfirm(driver: ThenableWebDriver) {
  await driver.wait(
    until.elementLocated(By.id("confirmRegisterButton")),
    25_000
  );
}

async function on_RegisterConfirm_Confirm(driver: ThenableWebDriver) {
  await driver.findElement(By.id("confirmRegisterButton")).click();
}

// View: Register Show Number

async function on_RegisterShowNumber(
  driver: ThenableWebDriver
): Promise<string> {
  await driver.wait(until.elementLocated(By.id("displayUserContinue")), 15_000);
  return await driver.findElement(By.className("highlightBox")).getText();
}

async function on_RegisterShowNumber_Continue(driver: ThenableWebDriver) {
  await driver.findElement(By.id("displayUserContinue")).click();
}

async function on_RegisterShowNumber_Fixup(driver: ThenableWebDriver) {
  const elem = await driver.findElement(By.className("highlightBox"));
  await driver.executeScript(
    "arguments[0].innerText = arguments[1];",
    elem,
    "12345"
  );
}

// View: Single device login warning
async function on_SingleDeviceLoginWarning(driver: ThenableWebDriver) {
  await driver.wait(
    until.elementLocated(By.id("displayWarningPrimary")),
    3_000
  );
}

async function on_SingleDeviceLoginWarning_Continue(driver: ThenableWebDriver) {
  await driver.findElement(By.id("displayWarningPrimary")).click();
}

// View: Recovery method selector
async function on_RecoveryMethodSelector(driver: ThenableWebDriver) {
  await driver.wait(until.elementLocated(By.id("skipRecovery")), 3_000);
}

async function on_RecoveryMethodSelector_Skip(driver: ThenableWebDriver) {
  await driver.findElement(By.id("skipRecovery")).click();
}

// View: Main view

async function on_Main(device_name: string, driver: ThenableWebDriver) {
  // wait for device list to load
  await driver.wait(
    until.elementLocated(By.xpath(`//div[string()='${device_name}']`)),
    10_000
  );
}

async function on_Main_AddAdditionalDevice(driver: ThenableWebDriver) {
  await driver.findElement(By.id("addAdditionalDevice")).click();
}

async function on_Main_Logout(driver: ThenableWebDriver) {
  await driver.findElement(By.id("logoutButton")).click();
}

async function on_Main_Fixup(driver: ThenableWebDriver) {
  // replace the Identity Anchor for a reproducible screenshot
  const elem = await driver.findElement(By.className("highlightBox"));
  await driver.executeScript(
    "arguments[0].innerText = arguments[1];",
    elem,
    "12345"
  );
}

async function on_Main_Remove(device_name: string, driver: ThenableWebDriver) {
  await driver
    .findElement(
      By.xpath(`//div[string()='${device_name}']/following-sibling::button`)
    )
    .click();
}

// View: Welcome back

async function on_WelcomeBack(driver: ThenableWebDriver): Promise<string> {
  await driver.wait(until.elementLocated(By.id("loginDifferent")), 15_000);
  return await driver.findElement(By.className("highlightBox")).getText();
}

async function on_WelcomeBack_Fixup(driver: ThenableWebDriver) {
  const elem = await driver.findElement(By.className("highlightBox"));
  await driver.executeScript(
    "arguments[0].innerText = arguments[1];",
    elem,
    "12345"
  );
}

async function on_WelcomeBack_Login(driver: ThenableWebDriver) {
  await driver.findElement(By.id("login")).click();
}

// View: Add device Identity Anchor
async function on_AddDeviceUserNumber(
  driver: ThenableWebDriver
): Promise<string> {
  return await driver
    .wait(until.elementLocated(By.id("addDeviceUserNumber")), 3_000)
    .getAttribute("value");
}

async function on_AddDeviceUserNumber_Continue(
  driver: ThenableWebDriver,
  user_number?: string
) {
  if (user_number !== undefined) {
    await fillText(driver, "addDeviceUserNumber", user_number);
  }
  await driver.findElement(By.id("addDeviceUserNumberContinue")).click();
}

async function on_AddDeviceUserNumber_Fixup(driver: ThenableWebDriver) {
  // replace the Identity Anchor for a reproducible screenshot
  const elem = await driver.findElement(By.id("addDeviceUserNumber"));
  await driver.executeScript(
    "arguments[0].value = arguments[1];",
    elem,
    "12345"
  );
}

// View: Add device

async function on_AddDevice(driver: ThenableWebDriver): Promise<string> {
  return await driver
    .wait(until.elementLocated(By.id("linkText")), 3_000)
    .getAttribute("value");
}

async function on_AddDevice_Fixup(driver: ThenableWebDriver) {
  const elem = await driver.wait(
    until.elementLocated(By.id("linkText")),
    3_000
  );
  await driver.executeScript(
    "arguments[0].value = arguments[1];",
    elem,
    "(link removed from screenshot)"
  );
}

// View: Add device confirm

async function on_AddDeviceConfirm(driver: ThenableWebDriver) {
  await driver.wait(until.elementLocated(By.id("addDevice")), 3_000);
}

async function on_AddDeviceConfirm_Confirm(driver: ThenableWebDriver) {
  await driver.findElement(By.id("addDevice")).click();
}

async function on_AddDeviceConfirm_Fixup(driver: ThenableWebDriver) {
  const userNumberElem = await driver.findElement(By.className("highlightBox"));
  await driver.executeScript(
    "arguments[0].innerText = arguments[1];",
    userNumberElem,
    "12345"
  );
}

// View: Add device alias

async function on_AddDeviceAlias(driver: ThenableWebDriver) {
  await driver.wait(until.elementLocated(By.id("deviceAliasContinue")), 3_000);
}

async function on_AddDeviceAlias_Type(
  alias: string,
  driver: ThenableWebDriver
) {
  await driver.findElement(By.id("deviceAlias")).sendKeys(alias);
}

async function on_AddDeviceAlias_Continue(driver: ThenableWebDriver) {
  await driver.findElement(By.id("deviceAliasContinue")).click();
}

// View: Add device success

async function on_AddDeviceSuccess(driver: ThenableWebDriver) {
  await driver.wait(until.elementLocated(By.id("manageDevicesButton")), 10_000);
}

// View: Authorize application

async function on_AuthApp(driver: ThenableWebDriver) {
  await driver.wait(until.elementLocated(By.id("confirmRedirect")), 5_000);
}

async function on_AuthApp_Confirm(driver: ThenableWebDriver) {
  await driver.findElement(By.id("confirmRedirect")).click();
}

// View: About

async function on_About(driver: ThenableWebDriver) {
  await driver.wait(until.elementLocated(By.id("about")), 3_000);
}

// View: Compatibility notice

async function on_CompatibilityNotice(driver: ThenableWebDriver) {
  await driver.wait(until.elementLocated(By.id("compatibilityNotice")), 3_000);
}

/*
## Demoapp helpers
*/

async function on_DemoApp(driver: ThenableWebDriver): Promise<string> {
  return await driver
    .wait(until.elementLocated(By.id("principal")), 10_000)
    .getText();
}

async function openDemoApp(driver: ThenableWebDriver) {
  await driver.get(DEMO_APP_URL);
  await fillText(driver, "iiUrl", II_URL);
}

async function on_DemoApp_Signin(driver: ThenableWebDriver) {
  await driver.findElement(By.id("signinBtn")).click();
}

async function on_DemoApp_SetMaxTimeToLive(
  driver: ThenableWebDriver,
  mttl: BigInt
) {
  await fillText(driver, "maxTimeToLive", String(mttl));
}

async function on_DemoApp_Whoami(driver: ThenableWebDriver) {
  await fillText(driver, "hostUrl", REPLICA_URL);
  await fillText(driver, "canisterId", WHOAMI_CANISTER);
  await driver.findElement(By.id("whoamiBtn")).click();
  const whoamiResponseElem = await driver.findElement(By.id("whoamiResponse"));
  await driver.wait(until.elementTextContains(whoamiResponseElem, "-"), 6_000);
  return await whoamiResponseElem.getText();
}

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
        .windowSize({ width: 1050, height: 1400 })
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

async function fillText(driver: ThenableWebDriver, id: string, text: string) {
  const elem = await driver.findElement(By.id(id));
  elem.clear();
  elem.sendKeys(text);
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
## Combined flows
*/

async function registerNewIdentity(driver: ThenableWebDriver): Promise<string> {
  await on_Welcome(driver);
  await on_Welcome_Register(driver);
  await on_Register(driver);
  await on_Register_TypeAliasEnter(DEVICE_NAME1, driver);
  await on_RegisterConfirm(driver);
  await on_RegisterConfirm_Confirm(driver);
  const userNumber = await on_RegisterShowNumber(driver);
  await on_RegisterShowNumber_Continue(driver);
  await on_SingleDeviceLoginWarning(driver);
  await on_SingleDeviceLoginWarning_Continue(driver);
  await on_RecoveryMethodSelector(driver);
  await on_RecoveryMethodSelector_Skip(driver);
  return userNumber;
}

async function login(userNumber: string, driver: ThenableWebDriver) {
  await on_Welcome(driver);
  await on_Welcome_TypeUserNumber(userNumber, driver);
  await on_Welcome_Login(driver);
  await on_SingleDeviceLoginWarning(driver);
  await on_SingleDeviceLoginWarning_Continue(driver);
  await on_RecoveryMethodSelector(driver);
  await on_RecoveryMethodSelector_Skip(driver);
  await on_Main(DEVICE_NAME1, driver);
}

/*
## The actual tests
*/
test("_Register new identity and login with it", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    await driver.get(II_URL);
    const userNumber = await registerNewIdentity(driver);
    await on_Main(DEVICE_NAME1, driver);
    await await on_Main_Logout(driver);
    await login(userNumber, driver);
  });
}, 300_000);

test("Register new identity and add additional device", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    const firstAuthenticator = await addVirtualAuthenticator(driver);
    await driver.get(II_URL);
    const userNumber = await registerNewIdentity(driver);
    await on_Main(DEVICE_NAME1, driver);
    // We're removing the first authenticator here, because unfortunately we
    // can't tell Chrome to _actually_ use the second authenticator, which
    // leads to flaky tests otherwise.
    await removeVirtualAuthenticator(driver, firstAuthenticator);
    const secondAuthenticator = await addVirtualAuthenticator(driver);
    await on_Main_AddAdditionalDevice(driver);

    await on_AddDeviceAlias(driver);
    await on_AddDeviceAlias_Type(DEVICE_NAME2, driver);
    await on_AddDeviceAlias_Continue(driver);

    await driver.sleep(10_000);

    await on_Main(DEVICE_NAME1, driver);
    await on_Main(DEVICE_NAME2, driver);

    await on_Main_Logout(driver);
    await login(userNumber, driver);
  });
}, 300_000);

test("Log into client application, after registration", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    await openDemoApp(driver);
    expect(await on_DemoApp(driver)).toBe("2vxsx-fae");
    await on_DemoApp_Signin(driver);
    await switchToPopup(driver);
    await registerNewIdentity(driver);
    await on_AuthApp(driver);
    await on_AuthApp_Confirm(driver);
    await waitToClose(driver);
    const principal = await on_DemoApp(driver);
    expect(principal).not.toBe("2vxsx-fae");
    expect(await on_DemoApp_Whoami(driver)).toBe(principal);
    // default value
    const exp = await driver.findElement(By.id("expiration")).getText();
    expect(Number(exp) / (30 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Delegation maxTimeToLive: 1 min", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    await openDemoApp(driver);
    expect(await on_DemoApp(driver)).toBe("2vxsx-fae");
    await on_DemoApp_SetMaxTimeToLive(driver, BigInt(60_000_000_000));
    await on_DemoApp_Signin(driver);
    await switchToPopup(driver);
    await registerNewIdentity(driver);
    await on_AuthApp(driver);
    await on_AuthApp_Confirm(driver);
    await waitToClose(driver);
    expect(await on_DemoApp(driver)).not.toBe("2vxsx-fae");
    const exp = await driver.findElement(By.id("expiration")).getText();
    // compare only up to one decimal place for the 1min test
    expect(Number(exp) / 60_000_000_000).toBeCloseTo(1, 0);
  });
}, 300_000);

test("Delegation maxTimeToLive: 1 day", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    await openDemoApp(driver);
    expect(await on_DemoApp(driver)).toBe("2vxsx-fae");
    await on_DemoApp_SetMaxTimeToLive(driver, BigInt(86400_000_000_000));
    await on_DemoApp_Signin(driver);
    await switchToPopup(driver);
    await registerNewIdentity(driver);
    await on_AuthApp(driver);
    await on_AuthApp_Confirm(driver);
    await waitToClose(driver);
    expect(await on_DemoApp(driver)).not.toBe("2vxsx-fae");
    const exp = await driver.findElement(By.id("expiration")).getText();
    expect(Number(exp) / 86400_000_000_000).toBeCloseTo(1);
  });
}, 300_000);

test("Delegation maxTimeToLive: 1 month", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    await openDemoApp(driver);
    expect(await on_DemoApp(driver)).toBe("2vxsx-fae");
    await on_DemoApp_SetMaxTimeToLive(driver, BigInt(2592000_000_000_000));
    await on_DemoApp_Signin(driver);
    await switchToPopup(driver);
    await registerNewIdentity(driver);
    await on_AuthApp(driver);
    await on_AuthApp_Confirm(driver);
    await waitToClose(driver);
    expect(await on_DemoApp(driver)).not.toBe("2vxsx-fae");
    const exp = await driver.findElement(By.id("expiration")).getText();
    // NB: Max out at 8 days
    expect(Number(exp) / 691200_000_000_000).toBeCloseTo(1);
  });
}, 300_000);

test("Screenshots", async () => {
  await run_in_browser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    await driver.get(II_URL);
    await wait_for_fonts(driver);

    await on_Welcome(driver);
    await screenshot("00-welcome", driver);
    await on_Welcome_Register(driver);
    await on_Register(driver);
    await screenshot("01-register", driver);
    await on_Register_TypeAliasEnter(DEVICE_NAME1, driver);
    await on_RegisterConfirm(driver);
    await screenshot("02-register-confirm", driver);
    await on_RegisterConfirm_Confirm(driver);
    const userNumber = await on_RegisterShowNumber(driver);
    await on_RegisterShowNumber_Fixup(driver);
    await screenshot("03-register-user-number", driver);
    await on_RegisterShowNumber_Continue(driver);
    await on_SingleDeviceLoginWarning(driver);
    await screenshot("17-single-device-warning", driver);
    await on_SingleDeviceLoginWarning_Continue(driver);
    await on_RecoveryMethodSelector(driver);
    await screenshot("18-recover-method-selector", driver);
    await on_RecoveryMethodSelector_Skip(driver);
    await on_Main(DEVICE_NAME1, driver);
    await on_Main_Fixup(driver);
    await screenshot("04-main", driver);
    await on_Main_Logout(driver);
    await on_Welcome(driver); // no point taking screenshot
    await on_Welcome_TypeUserNumber(userNumber, driver);
    await on_Welcome_Login(driver);
    await on_SingleDeviceLoginWarning(driver);
    await on_SingleDeviceLoginWarning_Continue(driver);
    await on_RecoveryMethodSelector(driver);
    await on_RecoveryMethodSelector_Skip(driver);
    await on_Main(DEVICE_NAME1, driver);

    await driver.get(II_URL);
    const userNumber2 = await on_WelcomeBack(driver);
    expect(userNumber2).toBe(userNumber);
    await on_WelcomeBack_Fixup(driver);
    await screenshot("05-welcome-back", driver);
    await on_WelcomeBack_Login(driver);
    await on_SingleDeviceLoginWarning(driver);
    await on_SingleDeviceLoginWarning_Continue(driver);
    await on_RecoveryMethodSelector(driver);
    await on_RecoveryMethodSelector_Skip(driver);
    await on_Main(DEVICE_NAME1, driver);

    // Now the link device flow, using a second browser
    await run_in_nested_browser(async (driver2) => {
      await addVirtualAuthenticator(driver2);
      await driver2.get(II_URL);
      await on_Welcome(driver2);
      await on_Welcome_TypeUserNumber(userNumber, driver2);
      await on_Welcome_AddDevice(driver2);
      await on_AddDeviceUserNumber(driver2);
      await on_AddDeviceUserNumber_Fixup(driver2);
      await screenshot("06-new-device-user-number", driver2);
      await on_AddDeviceUserNumber_Continue(driver2, userNumber);
      const link = await on_AddDevice(driver2);
      console.log("The add device link is", link);
      await on_AddDevice_Fixup(driver2);
      await screenshot("07-new-device", driver2);

      // Log in with previous browser again
      await driver.get("about:blank");
      await driver.get(link);
      await wait_for_fonts(driver);
      await on_WelcomeBack(driver);
      await on_WelcomeBack_Fixup(driver);
      await screenshot("08-new-device-login", driver);
      await on_WelcomeBack_Login(driver);
      await on_SingleDeviceLoginWarning(driver);
      await on_SingleDeviceLoginWarning_Continue(driver);
      await on_RecoveryMethodSelector(driver);
      await on_RecoveryMethodSelector_Skip(driver);
      await on_AddDeviceConfirm(driver);
      await on_AddDeviceConfirm_Fixup(driver);
      await screenshot("09-new-device-confirm", driver);
      await on_AddDeviceConfirm_Confirm(driver);
      await on_AddDeviceAlias(driver);
      await screenshot("10-new-device-alias", driver);
      await on_AddDeviceAlias_Type(DEVICE_NAME2, driver);
      await on_AddDeviceAlias_Continue(driver);
      await on_AddDeviceSuccess(driver);
      await screenshot("11-new-device-done", driver);

      // Back to other browser, should be a welcome view now
      await on_WelcomeBack(driver2);
      await on_WelcomeBack_Fixup(driver2);
      await screenshot("12-new-device-login", driver2);
      await on_WelcomeBack_Login(driver2);
      await on_SingleDeviceLoginWarning(driver2);
      await on_SingleDeviceLoginWarning_Continue(driver2);
      await on_RecoveryMethodSelector(driver2);
      await on_RecoveryMethodSelector_Skip(driver2);
      await on_Main(DEVICE_NAME2, driver2);
      await on_Main_Fixup(driver2);
      await screenshot("13-new-device-listed", driver2);

      // Try to remove current device
      await on_Main_Remove(DEVICE_NAME2, driver2);
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
    await on_About(driver);
    await screenshot("14-about", driver);

    // Test device removal
    await driver.get(II_URL);
    const userNumber3 = await on_WelcomeBack(driver);
    expect(userNumber3).toBe(userNumber);
    await on_WelcomeBack_Login(driver);
    await on_SingleDeviceLoginWarning(driver);
    await on_SingleDeviceLoginWarning_Continue(driver);
    await on_RecoveryMethodSelector(driver);
    await on_RecoveryMethodSelector_Skip(driver);
    await on_Main(DEVICE_NAME2, driver);
    const buttonElem2 = await driver.findElement(
      By.xpath(`//div[string()='${DEVICE_NAME2}']/following-sibling::button`)
    );
    await on_Main_Remove(DEVICE_NAME2, driver);
    // No dialog here!

    await driver.wait(until.stalenessOf(buttonElem2));
    await on_Main(DEVICE_NAME1, driver);
    await on_Main_Fixup(driver);
    await screenshot("15-after-removal", driver);

    await on_Main_Remove(DEVICE_NAME1, driver);
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
    await on_Main(DEVICE_NAME1, driver);

    // Compatibility notice page
    await driver.get("about:blank");
    await driver.get(II_URL + "#compatibilityNotice");
    await wait_for_fonts(driver);
    await on_CompatibilityNotice(driver);
    await screenshot("16-compatibility-notice", driver);
  });
}, 400_000);
