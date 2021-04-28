import { Builder, By, until, ThenableWebDriver, Key, logging} from 'selenium-webdriver';
import { Executor, Command } from 'selenium-webdriver/lib/command';
import { Options as ChromeOptions } from 'selenium-webdriver/chrome';
import { writeFile } from 'fs/promises';

/*
## Structure of this file

 - Constants

 - Assertions and actions, grouped by View

   - nowOnX() assertions:
     These wait for a characteristic element of view `X`
     Some return useful data from view `X`

   - onXY() actions:
     These should be run on `X`, and they perform the action `Y`
     Most actions will go to a different view, but not all of them.

   - onXFixup() actions:
     These should be run on view `X`, and they replace variable data
     (mostly usernames) with fixed data for screenshots

 - Setup helpers (getting web driver, waiting for fonts to be loaded)

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

async function nowOnWelcome(driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('registerUserNumber')), 3_000);
}

async function onWelcomeTypeUserNumber(user_number: string, driver: ThenableWebDriver) {
    await driver.findElement(By.id('registerUserNumber'), 3_000).sendKeys(user_number);
}

async function onWelcomeLogin(driver: ThenableWebDriver) {
    await driver.findElement(By.id('loginButton')).click();
}

async function onWelcomeRegister(driver: ThenableWebDriver) {
    await driver.findElement(By.id('registerButton')).click();
}

async function onWelcomeAddDevice(driver: ThenableWebDriver) {
    await driver.findElement(By.id('addNewDeviceButton')).click();
}

// View: Register

async function nowOnRegister(driver: ThenableWebDriver) {
}

async function onRegisterTypeAliasEnter(alias : string, driver: ThenableWebDriver) {
    await driver.findElement(By.id('registerAlias')).sendKeys(alias, Key.RETURN);
}

// View: Register confirmation

async function nowOnRegisterConfirm(driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('confirmRegisterButton')), 5_000);
}

async function onRegisterConfirmConfirm(driver: ThenableWebDriver) {
    await driver.findElement(By.id('confirmRegisterButton')).click();
}

// View: Register Show Number

async function nowOnRegisterShowNumber(driver: ThenableWebDriver) : Promise<string> {
  await driver.wait(until.elementLocated(By.id('displayUserContinue')), 15_000);
  return await driver.findElement(By.className("highlightBox")).getText();
}

async function onRegisterShowNumberContinue(driver: ThenableWebDriver) {
  await driver.findElement(By.id('displayUserContinue')).click();
}

async function onRegisterShowNumberFixup(driver: ThenableWebDriver) {
  const elem = await driver.findElement(By.className("highlightBox"));
  await driver.executeScript("arguments[0].innerText = arguments[1];", elem, '12345');
}

// View: Main view

async function nowOnMain(device_name : string, driver : ThenableWebDriver) {
    // wait for device list to load
    await driver.wait(until.elementLocated(By.xpath(`//span[string()='${device_name}']`)), 3_000);
}

async function onMainLogout(driver : ThenableWebDriver) {
    await driver.findElement(By.id('logoutButton')).click();
}

async function onMainFixup(driver : ThenableWebDriver) {
    // replace the user number for a reproducible screenshot
    let elem = await driver.findElement(By.id('userNumberSpan'));
    await driver.executeScript("arguments[0].innerText = arguments[1];", elem, '12345');
}

// View: Welcome back

async function nowOnWelcomeBack(driver: ThenableWebDriver) : Promise<string> {
  await driver.wait(until.elementLocated(By.id('loginDifferent')), 15_000);
  return await driver.findElement(By.className("highlightBox")).getText();
}

async function onWelcomeBackFixup(driver: ThenableWebDriver) {
  const elem = await driver.findElement(By.className("highlightBox"));
  await driver.executeScript("arguments[0].innerText = arguments[1];", elem, '12345');
}

async function onWelcomeBackLogin(driver: ThenableWebDriver) {
    await driver.findElement(By.id('login')).click();
}

// View: Add device

async function nowOnAddDevice(driver: ThenableWebDriver): Promise<string> {
  return await driver.wait(until.elementLocated(By.id("linkText")), 3_000).getAttribute('value');
}

async function onAddDeviceFixup(driver : ThenableWebDriver) {
  const elem = await driver.wait(until.elementLocated(By.id("linkText")), 3_000);
  await driver.executeScript("arguments[0].value = arguments[1];", elem, '(link removed from screenshot)');
}

test('Screenshots', async () => {
    await run_in_browser_with_virtual_authenticator(async (driver) => {
        await driver.get(IDP_SERVICE_URL);
        await wait_for_fonts(driver);

	await nowOnWelcome(driver);
        await screenshot('00-welcome', driver);
	await onWelcomeRegister(driver);
	await nowOnRegister(driver);
        await screenshot('01-register', driver);
        await onRegisterTypeAliasEnter(DEVICE_NAME1, driver);
        await nowOnRegisterConfirm(driver);
        await screenshot('02-register-confirm', driver);
        await onRegisterConfirmConfirm(driver);
        const userNumber = await nowOnRegisterShowNumber(driver);
        await onRegisterShowNumberFixup(driver);
        await screenshot('03-register-user-number', driver);
        await onRegisterShowNumberContinue(driver);
        await nowOnMain(DEVICE_NAME1, driver);
        await onMainFixup(driver);
        await screenshot('04-main', driver);
        await onMainLogout(driver);
        await nowOnWelcome(driver); // no point taking screenshot
        await onWelcomeTypeUserNumber(userNumber, driver);
        await onWelcomeLogin(driver);
        await nowOnMain(DEVICE_NAME1, driver);

        await driver.get(IDP_SERVICE_URL);
        const userNumber2 = await nowOnWelcomeBack(driver);
        expect(userNumber2).toBe(userNumber);
        await onWelcomeBackFixup(driver);
        await screenshot('05-welcome-back', driver);
        await onWelcomeBackLogin(driver);
        await nowOnMain(DEVICE_NAME1, driver);

        // Now the link device flow, using a second browser
        await run_in_browser_with_virtual_authenticator (async (driver2) => {
            await driver2.get(IDP_SERVICE_URL);
	    await nowOnWelcome(driver2);
            await onWelcomeTypeUserNumber(userNumber, driver2);
            await onWelcomeAddDevice(driver2);
            const link = await nowOnAddDevice(driver2);
            console.log('The add device link is', link);
            await onAddDeviceFixup(driver2);
            await screenshot('06-new-device', driver2);

            // Log in with previous browser again
            await driver.get('about:blank');
            await driver.get(link);
            await wait_for_fonts(driver);

            // Welcome back flow
            await nowOnWelcomeBack(driver);
            await onWelcomeBackFixup(driver);
            await screenshot('07-new-device-login', driver);
            await onWelcomeBackLogin(driver);

            await driver.wait(until.elementLocated(By.id('addDevice')));
            let userNumberElem = await driver.findElement(By.className("highlightBox"));
            await driver.executeScript("arguments[0].innerText = arguments[1];", userNumberElem, '12345');
            await screenshot('08-new-device-confirm', driver);


            await driver.findElement(By.id('addDevice')).click();
            await driver.wait(until.elementLocated(By.id('deviceAliasContinue')));
            await screenshot('09-new-device-alias', driver);

            await driver.findElement(By.id('deviceAlias')).sendKeys(DEVICE_NAME2, Key.RETURN);
            await driver.findElement(By.id('deviceAliasContinue')).click();
            await driver.sleep(5000);
            await screenshot('10-new-device-done', driver);

            // other browser redirects to welcome back view
            await driver2.wait(until.elementLocated(By.id('login')));
            userNumberElem = await driver2.findElement(By.className("highlightBox"));
            await driver2.executeScript("arguments[0].innerText = arguments[1];", userNumberElem, '12345');
            await screenshot('11-new-device-login', driver2);

            // we login
            await driver2.findElement(By.id('login')).click();

            // and now we see the new device
            await driver2.wait(until.elementLocated(By.xpath(`//span[string()='${DEVICE_NAME2}']`)), 3_000);
            let h3 = await driver2.wait(until.elementLocated(By.xpath("//h3[string()='Your User Number is "+userNumber+"']")), 15_000);
            await driver2.executeScript("arguments[0].innerText = arguments[1];", h3, 'Your User Number is 12345');
            await screenshot('12-new-device-listed', driver2);
        })
    })
}, 300_000);


test('Register new identity and login with it', async () => {
    await run_in_browser_with_virtual_authenticator(async (driver) => {
        await driver.get(IDP_SERVICE_URL);
        let userNumber = await registerNewIdentity(driver);
        await logout(driver);
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
        await driver.findElement(By.xpath("//button[text()='Yes']")).click();

        // check that we are indeed being redirected back
        let principal = await driver.wait(until.elementLocated(By.id('principal')), 10_000).getText();
        expect(principal).not.toBe('2vxsx-fae');
        // TODO: Use a whoami service to check that loggin in works
    })
}, 300_000);

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

async function registerNewIdentity(driver: ThenableWebDriver): Promise<string> {
    await driver.wait(until.elementLocated(By.id('registerButton')), 5_000).click();
    await driver.findElement(By.id('registerAlias')).sendKeys(DEVICE_NAME1, Key.RETURN);
    await driver.wait(until.elementLocated(By.id('confirmRegisterButton')), 5_000);
    await driver.findElement(By.id('confirmRegisterButton')).click();

    let continueButton = await driver.wait(until.elementLocated(By.id('displayUserContinue')), 15_000);
    let userNumber = await driver.findElement(By.className("highlightBox")).getText();
    await continueButton.click();
    return userNumber;
}

async function logout(driver: ThenableWebDriver) {
    await driver.findElement(By.id('logoutButton')).click();
}

async function login(userNumber: string, driver: ThenableWebDriver) {
    await driver.wait(until.elementLocated(By.id('registerUserNumber')), 5_000).sendKeys(userNumber, Key.RETURN);
    await driver.findElement(By.id('loginButton')).click();
    await driver.wait(until.elementLocated(By.xpath("//h3[string()='Your User Number is "+userNumber+"']")), 15_000);
}

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
