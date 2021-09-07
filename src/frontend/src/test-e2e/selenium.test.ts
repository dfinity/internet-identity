import { By, ThenableWebDriver, until, } from "selenium-webdriver";
import {
  AboutView,
  AddDeviceAliasView,
  AddDeviceView,
  AddIdentityAnchorView,
  AuthorizeAppView,
  CompatabilityNoticeView,
  DemoAppView,
  MainView,
  RecoveryMethodSelectorView,
  RegisterView,
  SingleDeviceWarningView,
  WelcomeBackView,
  WelcomeView,
} from "./views";
import { FLOWS } from "./flows";
import {
  addVirtualAuthenticator,
  removeVirtualAuthenticator,
  runInBrowser,
  runInNestedBrowser,
  screenshot,
  switchToPopup,
  waitForFonts,
  waitToClose
} from "./util";

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import canister_ids1 from "../../../../.dfx/local/canister_ids.json";
import canister_ids2 from "../../../../demos/whoami/.dfx/local/canister_ids.json";

const IDENTITY_CANISTER = canister_ids1.internet_identity.local;
const WHOAMI_CANISTER = canister_ids2.whoami.local;

const REPLICA_URL = "http://localhost:8000";
const II_URL = `http://localhost:8000/?canisterId=${IDENTITY_CANISTER}`;
const DEMO_APP_URL = "http://localhost:8080/";

const DEVICE_NAME1 = "Virtual WebAuthn device";
const DEVICE_NAME2 = "Other WebAuthn device";

test("_Register new identity and login with it", async () => {
  await runInBrowser(async (driver: ThenableWebDriver) => {
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
  await runInBrowser(async (driver: ThenableWebDriver) => {
    const firstAuthenticator = await addVirtualAuthenticator(driver);
    await driver.get(II_URL);
    const userNumber = await FLOWS.registerNewIdentity(DEVICE_NAME1, driver);

    const mainView = new MainView(driver);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    // We're removing the first authenticator here, because unfortunately we
    // can't tell Chrome to _actually_ use the second authenticator, which
    // leads to flaky tests otherwise.
    await removeVirtualAuthenticator(driver, firstAuthenticator);
    await addVirtualAuthenticator(driver);
    await mainView.addAdditionalDevice();

    const addDeviceAliasView = new AddDeviceAliasView(driver);
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
  await runInBrowser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    const demoAppView = new DemoAppView(driver);
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
    expect(await demoAppView.whoami(REPLICA_URL, WHOAMI_CANISTER)).toBe(
      principal
    );
    // default value
    const exp = await driver.findElement(By.id("expiration")).getText();
    expect(Number(exp) / (30 * 60_000_000_000)).toBeCloseTo(1);
  });
}, 300_000);

test("Delegation maxTimeToLive: 1 min", async () => {
  await runInBrowser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    const demoAppView = new DemoAppView(driver);
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
  await runInBrowser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    const demoAppView = new DemoAppView(driver);
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
  await runInBrowser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    const demoAppView = new DemoAppView(driver);
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
  await runInBrowser(async (driver: ThenableWebDriver) => {
    await addVirtualAuthenticator(driver);
    await driver.get(II_URL);

    await waitForFonts(driver);
    const welcomeView = new WelcomeView(driver);
    await welcomeView.waitForDisplay();
    await screenshot("00-welcome", driver);
    await welcomeView.register();
    const registerView = new RegisterView(driver);
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
    const singleDeviceWarningView = new SingleDeviceWarningView(driver);
    await singleDeviceWarningView.waitForDisplay();
    await screenshot("17-single-device-warning", driver);
    await singleDeviceWarningView.continue();
    const recoveryMethodSelectorView = new RecoveryMethodSelectorView(driver);
    await recoveryMethodSelectorView.waitForDisplay();
    await screenshot("18-recover-method-selector", driver);
    await recoveryMethodSelectorView.skip();
    const mainView = new MainView(driver);
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);
    await mainView.fixup();
    await screenshot("04-main", driver);
    await mainView.logout();
    await welcomeView.waitForDisplay(); // no point taking screenshot
    await welcomeView.typeUserNumber(userNumber);
    await welcomeView.login();
    await singleDeviceWarningView.waitForDisplay();
    await singleDeviceWarningView.continue();
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skip();
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
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skip();
    await mainView.waitForDeviceDisplay(DEVICE_NAME1);

    // Now the link device flow, using a second browser
    await runInNestedBrowser(async (driver2) => {
      await addVirtualAuthenticator(driver2);
      await driver2.get(II_URL);
      const welcomeView2 = new WelcomeView(driver2);
      await welcomeView2.waitForDisplay();
      await welcomeView2.typeUserNumber(userNumber);
      await welcomeView2.addDevice();
      const addIdentityAnchorView2 = new AddIdentityAnchorView(driver2);
      await addIdentityAnchorView2.waitForDisplay();
      await addIdentityAnchorView2.fixup();
      await screenshot("06-new-device-user-number", driver2);
      await addIdentityAnchorView2.continue(userNumber);
      const addDeviceView2 = new AddDeviceView(driver2);
      await addDeviceView2.waitForDisplay();

      const link = await addDeviceView2.getLinkText();
      console.log("The add device link is", link);
      await addDeviceView2.fixup();
      await screenshot("07-new-device", driver2);

      // Log in with previous browser again
      await driver.get("about:blank");
      await driver.get(link);
      await waitForFonts(driver);
      const welcomeBackView = new WelcomeBackView(driver);
      await welcomeBackView.waitForDisplay();
      await welcomeBackView.fixup();
      await screenshot("08-new-device-login", driver);
      await welcomeBackView.login();
      await singleDeviceWarningView.waitForDisplay();
      await singleDeviceWarningView.continue();
      await recoveryMethodSelectorView.waitForDisplay();
      await recoveryMethodSelectorView.skip();
      const addDeviceView = new AddDeviceView(driver);
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
      const singleDeviceWarningView2 = new SingleDeviceWarningView(driver2);
      await singleDeviceWarningView2.waitForDisplay();
      await singleDeviceWarningView2.continue();
      const recoveryMethodSelectorView2 = new RecoveryMethodSelectorView(
        driver2
      );
      await recoveryMethodSelectorView2.waitForDisplay();
      await recoveryMethodSelectorView2.skip();
      const mainView2 = new MainView(driver2);
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
    await waitForFonts(driver);
    const aboutView = new AboutView(driver);
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
    await recoveryMethodSelectorView.waitForDisplay();
    await recoveryMethodSelectorView.skip();
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
    await waitForFonts(driver);
    const compatabilityNoticeView = new CompatabilityNoticeView(driver);
    await compatabilityNoticeView.waitForDisplay();
    await screenshot("16-compatibility-notice", driver);
  });
}, 400_000);
