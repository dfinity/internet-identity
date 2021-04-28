import { Builder, By, until, ThenableWebDriver, Key, logging} from 'selenium-webdriver';
import { Executor, Command } from 'selenium-webdriver/lib/command';
import { Options as ChromeOptions } from 'selenium-webdriver/chrome';
import { writeFile } from 'fs/promises';

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
import canister_ids from '../../../.dfx/local/canister_ids.json';
const IDENTITY_CANISTER = canister_ids.idp_service.local;

const REPLICA_URL = 'http://localhost:8000';
const IDP_SERVICE_URL = `http://localhost:8000/?canisterId=${IDENTITY_CANISTER}`
const IDP_AUTH_URL = `http://localhost:8000/authorize?canisterId=${IDENTITY_CANISTER}`
const DEMO_APP_URL = 'http://localhost:8080/';

const DEVICE_NAME1 = 'Virtual WebAuthn device';
const DEVICE_NAME2 = 'Other WebAuthn device';

/*
## Per-view helpers
*/

// View: Welcome

async function on_Welcome(driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('registerUserNumber')), 3_000);
}

async function on_Welcome_TypeUserNumber(user_number: string, driver: ThenableWebDriver) {
    await driver.findElement(By.id('registerUserNumber'), 3_000).sendKeys(user_number);
}

async function on_Welcome_Login(driver: ThenableWebDriver) {
    await driver.findElement(By.id('loginButton')).click();
}

async function on_Welcome_Register(driver: ThenableWebDriver) {
    await driver.findElement(By.id('registerButton')).click();
}

async function on_Welcome_AddDevice(driver: ThenableWebDriver) {
    await driver.findElement(By.id('addNewDeviceButton')).click();
}

// View: _Register

async function on_Register(driver: ThenableWebDriver) {
}

async function on_Register_TypeAliasEnter(alias : string, driver: ThenableWebDriver) {
    await driver.findElement(By.id('registerAlias')).sendKeys(alias, Key.RETURN);
}

// View: _Register confirmation

async function on_RegisterConfirm(driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('confirmRegisterButton')), 5_000);
}

async function on_RegisterConfirm_Confirm(driver: ThenableWebDriver) {
    await driver.findElement(By.id('confirmRegisterButton')).click();
}

// View: _Register Show Number

async function on_RegisterShowNumber(driver: ThenableWebDriver) : Promise<string> {
  // here Proof of work happens, so extra long wait time
  await driver.wait(until.elementLocated(By.id('displayUser_Continue')), 30_000);
  return await driver.findElement(By.className("highlightBox")).getText();
}

async function on_RegisterShowNumber_Continue(driver: ThenableWebDriver) {
  await driver.findElement(By.id('displayUser_Continue')).click();
}

async function on_RegisterShowNumber_Fixup(driver: ThenableWebDriver) {
  const elem = await driver.findElement(By.className("highlightBox"));
  await driver.executeScript("arguments[0].innerText = arguments[1];", elem, '12345');
}

// View: Main view

async function on_Main(device_name : string, driver : ThenableWebDriver) {
    // wait for device list to load
    await driver.wait(until.elementLocated(By.xpath(`//span[string()='${device_name}']`)), 3_000);
}

async function on_Main_Logout(driver : ThenableWebDriver) {
    await driver.findElement(By.id('logoutButton')).click();
}

async function on_Main_Fixup(driver : ThenableWebDriver) {
    // replace the user number for a reproducible screenshot
    let elem = await driver.findElement(By.id('userNumberSpan'));
    await driver.executeScript("arguments[0].innerText = arguments[1];", elem, '12345');
}

// View: Welcome back

async function on_WelcomeBack(driver: ThenableWebDriver) : Promise<string> {
    await driver.wait(until.elementLocated(By.id('loginDifferent')), 15_000);
    return await driver.findElement(By.className("highlightBox")).getText();
}

async function on_WelcomeBack_Fixup(driver: ThenableWebDriver) {
    const elem = await driver.findElement(By.className("highlightBox"));
    await driver.executeScript("arguments[0].innerText = arguments[1];", elem, '12345');
}

async function on_WelcomeBack_Login(driver: ThenableWebDriver) {
    await driver.findElement(By.id('login')).click();
}

// View: Add device

async function on_AddDevice(driver: ThenableWebDriver): Promise<string> {
    return await driver.wait(until.elementLocated(By.id("linkText")), 3_000).getAttribute('value');
}

async function on_AddDevice_Fixup(driver : ThenableWebDriver) {
    const elem = await driver.wait(until.elementLocated(By.id("linkText")), 3_000);
    await driver.executeScript("arguments[0].value = arguments[1];", elem, '(link removed from screenshot)');
}

// View: Add device confirm

async function on_AddDeviceConfirm(driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('addDevice')), 3_000);
}

async function on_AddDeviceConfirm_Confirm(driver: ThenableWebDriver) {
    await driver.findElement(By.id('addDevice')).click();
}

async function on_AddDeviceConfirm_Fixup(driver : ThenableWebDriver) {
    const userNumberElem = await driver.findElement(By.className("highlightBox"));
    await driver.executeScript("arguments[0].innerText = arguments[1];", userNumberElem, '12345');
}

// View: Add device alias

async function on_AddDeviceAlias(driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('deviceAlias_Continue')), 3_000);
}

async function on_AddDeviceAlias_Type(alias : string, driver: ThenableWebDriver) {
    await driver.findElement(By.id('deviceAlias')).sendKeys(alias);
}

async function on_AddDeviceAlias_Continue(driver: ThenableWebDriver) {
    await driver.findElement(By.id('deviceAlias_Continue')).click();
}

// View: Add device success

async function on_AddDeviceSuccess(driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('manageDevicesButton')), 10_000);
}

// View: Authorize application

async function on_AuthApp(driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('confirmRedirect')), 5_000);
}

async function on_AuthApp_Confirm(driver: ThenableWebDriver) {
    await driver.findElement(By.id('confirmRedirect')).click();
}


/*
## Setup helpers
*/

async function addVirtualAuthenticator(executor: Executor, sessionId: string) {
    executor.defineCommand("AddVirtualAuthenticator", "POST", "/session/:sessionId/webauthn/authenticator");
    const cmd = new Command('AddVirtualAuthenticator');
    cmd.setParameter('protocol', 'ctap2');
    cmd.setParameter('transport', 'usb');
    cmd.setParameter('hasResidentKey', true);
    cmd.setParameter('isUserConsenting', true);
    cmd.setParameter('sessionId', sessionId);
    await executor.execute(cmd);
}

async function screenshot(name : string, driver: ThenableWebDriver) {
    let image = await driver.takeScreenshot();
    // writing to a subdirectory has the nice property that it fails if
    // this is run in the wrong directory
    await writeFile(`screenshots/${name}.png`, image, 'base64');
}

// Inspired by https://stackoverflow.com/a/66919695/946226
async function wait_for_fonts(driver : ThenableWebDriver) {
  for(var i = 0; i <= 10; i++) {
    if (await driver.executeScript("return document.fonts.status;") == "loaded") {
      return
    }
    driver.sleep(200);
  };
  console.log('Odd, document.font.status never reached state loaded, stuck at', await driver.executeScript("return document.fonts.status;"))
}

async function run_in_browser_with_virtual_authenticator(test) {
    const driver = new Builder().forBrowser('chrome')
        .setChromeOptions(new ChromeOptions()
          .headless() // hides the click show: uncomment to watch it
          .windowSize({width: 1024, height: 768})
        )
        .setLoggingPrefs(new logging.Preferences().setLevel('browser', 'all'))
        .build();
    try {
        const session = await driver.getSession();
        await addVirtualAuthenticator(driver.getExecutor(), session.getId());
        await test(driver);
    } catch (e) {
        console.log(await driver.manage().logs().get('browser'));
        console.log(await driver.getPageSource());
        throw e;
    } finally {
        // don’t quit, it seems to take down all of chrome, not just the current session
        // await driver.quit();
        await driver.close();
    }
};

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
    return userNumber;
}

async function login(userNumber: string, driver: ThenableWebDriver) {
    await on_Welcome(driver);
    await on_Welcome_TypeUserNumber(userNumber, driver);
    await on_Welcome_Login(driver);
    await on_Main(DEVICE_NAME1, driver);
}



/*
## The actual tests
*/
test('_Register new identity and login with it', async () => {
    await run_in_browser_with_virtual_authenticator(async (driver) => {
        await driver.get(IDP_SERVICE_URL);
        let userNumber = await registerNewIdentity(driver);
        await on_Main(DEVICE_NAME1, driver);
        await await on_Main_Logout(driver);
        await login(userNumber, driver);
    })
}, 300_000);

test('Log into client application, after registration', async () => {
    await run_in_browser_with_virtual_authenticator(async (driver) => {
        await driver.get(DEMO_APP_URL);
        await driver.findElement(By.id('idpUrl')).sendKeys(Key.CONTROL + "a");
        await driver.findElement(By.id('idpUrl')).sendKeys(Key.DELETE);
        await driver.findElement(By.id('idpUrl')).sendKeys(IDP_AUTH_URL);
        await driver.findElement(By.id('signinBtn')).click();

        let userNumber = await registerNewIdentity(driver);
        await on_AuthApp(driver);
        await on_AuthApp_Confirm(driver);

        // check that we are indeed being redirected back to demo app
        let principal = await driver.wait(until.elementLocated(By.id('principal')), 10_000).getText();
        expect(principal).not.toBe('2vxsx-fae');
        // TODO: Use a whoami service to check that logging in works
    })
}, 300_000);

test('Screenshots', async () => {
    await run_in_browser_with_virtual_authenticator(async (driver) => {
        await driver.get(IDP_SERVICE_URL);
        await wait_for_fonts(driver);

	await on_Welcome(driver);
        await screenshot('00-welcome', driver);
	await on_Welcome_Register(driver);
	await on_Register(driver);
        await screenshot('01-register', driver);
        await on_Register_TypeAliasEnter(DEVICE_NAME1, driver);
        await on_RegisterConfirm(driver);
        await screenshot('02-register-confirm', driver);
        await on_RegisterConfirm_Confirm(driver);
        const userNumber = await on_RegisterShowNumber(driver);
        await on_RegisterShowNumber_Fixup(driver);
        await screenshot('03-register-user-number', driver);
        await on_RegisterShowNumber_Continue(driver);
        await on_Main(DEVICE_NAME1, driver);
        await on_Main_Fixup(driver);
        await screenshot('04-main', driver);
        await on_Main_Logout(driver);
        await on_Welcome(driver); // no point taking screenshot
        await on_Welcome_TypeUserNumber(userNumber, driver);
        await on_Welcome_Login(driver);
        await on_Main(DEVICE_NAME1, driver);

        await driver.get(IDP_SERVICE_URL);
        const userNumber2 = await on_WelcomeBack(driver);
        expect(userNumber2).toBe(userNumber);
        await on_WelcomeBack_Fixup(driver);
        await screenshot('05-welcome-back', driver);
        await on_WelcomeBack_Login(driver);
        await on_Main(DEVICE_NAME1, driver);

        // Now the link device flow, using a second browser
        await run_in_browser_with_virtual_authenticator (async (driver2) => {
            await driver2.get(IDP_SERVICE_URL);
	    await on_Welcome(driver2);
            await on_Welcome_TypeUserNumber(userNumber, driver2);
            await on_Welcome_AddDevice(driver2);
            const link = await on_AddDevice(driver2);
            console.log('The add device link is', link);
            await on_AddDevice_Fixup(driver2);
            await screenshot('06-new-device', driver2);

            // Log in with previous browser again
            await driver.get('about:blank');
            await driver.get(link);
            await wait_for_fonts(driver);
            await on_WelcomeBack(driver);
            await on_WelcomeBack_Fixup(driver);
            await screenshot('07-new-device-login', driver);
            await on_WelcomeBack_Login(driver);
            await on_AddDeviceConfirm(driver);
            await on_AddDeviceConfirm_Fixup(driver);
            await screenshot('08-new-device-confirm', driver);
            await on_AddDeviceConfirm_Confirm(driver);
            await on_AddDeviceAlias(driver);
            await screenshot('09-new-device-alias', driver);
            await on_AddDeviceAlias_Type(DEVICE_NAME2, driver);
            await on_AddDeviceAlias_Continue(driver);
            await on_AddDeviceSuccess(driver);
            await screenshot('10-new-device-done', driver);

            // Back to other browser, should be a welcome view now
            await on_WelcomeBack(driver2);
            await on_WelcomeBack_Fixup(driver2);
            await screenshot('11-new-device-login', driver2);
            await on_WelcomeBack_Login(driver2);
            await on_Main(DEVICE_NAME2, driver2);
            await on_Main_Fixup(driver2);
            await screenshot('12-new-device-listed', driver2);
        })
    })
}, 300_000);


