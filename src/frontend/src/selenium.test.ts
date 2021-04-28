import { Builder, By, until, ThenableWebDriver, Key, logging} from 'selenium-webdriver';
import { Executor, Command } from 'selenium-webdriver/lib/command';
import { Options as ChromeOptions } from 'selenium-webdriver/chrome';
import { writeFile } from 'fs/promises';

// Read canister ids from the corresponding dfx files.
// This assumes that they have been successfully dfx-deployed
import canister_ids from '../../../.dfx/local/canister_ids.json';
const IDENTITY_CANISTER = canister_ids.idp_service.local;

const REPLICA_URL = 'http://localhost:8000';
const IDP_SERVICE_URL = `http://localhost:8000/?canisterId=${IDENTITY_CANISTER}`
const IDP_AUTH_URL = `http://localhost:8000/authorize?canisterId=${IDENTITY_CANISTER}`
const DEMO_APP_URL = 'http://localhost:8080/';

const DEVICE_NAME1 = 'Virtual WebAuthn device';

test('Screenshots', async () => {
    await run_in_browser_with_virtual_authenticator(async (driver) => {
        await driver.get(IDP_SERVICE_URL);
        await wait_for_fonts(driver);
        await screenshot('00-welcome', driver);

        await driver.wait(until.elementLocated(By.id('registerButton')), 3_000).click();
        await screenshot('01-register', driver);

        await driver.findElement(By.id('registerAlias')).sendKeys('Virtual WebAuthn device', Key.RETURN);
        // TODO: More reliable recognize this view
        await driver.wait(until.elementLocated(By.id('confirmRegisterButton')), 5_000);
        await screenshot('02-register-confirm', driver);

        await driver.findElement(By.id('confirmRegisterButton')).click();
        let continueButton = await driver.wait(until.elementLocated(By.id('displayUserContinue')), 15_000);
        let userNumberElem = await driver.findElement(By.className("highlightBox"));
        let userNumber = await userNumberElem.getText();
        // replace the user number for a reproducible screenshot
        await driver.executeScript("arguments[0].innerText = arguments[1];", userNumberElem, '12345');
        await screenshot('03-register-user-number', driver);

        await continueButton.click();
        // wait for device list to load
        await driver.wait(until.elementLocated(By.xpath(`//span[string()='${DEVICE_NAME1}']`)), 3_000);
        // replace the user number for a reproducible screenshot
        let h3 = await driver.wait(until.elementLocated(By.xpath("//h3[string()='Your User Number is "+userNumber+"']")), 15_000);
        await driver.executeScript("arguments[0].innerText = arguments[1];", h3, 'Your User Number is 12345');
        await screenshot('04-main', driver);

        await driver.findElement(By.id('logoutButton')).click();
        await driver.wait(until.elementLocated(By.id('registerButton')), 3_000);
        // should be at welcome again, no point taking screenshot

        await driver.findElement(By.id('registerUserNumber'), 3_000).sendKeys(userNumber);
        await driver.findElement(By.id('loginButton')).click();
        // wait for device list to load
        await driver.wait(until.elementLocated(By.xpath(`//span[string()='${DEVICE_NAME1}']`)), 3_000);
        // should be logged in again, no point taking screenshot

        await driver.get(IDP_SERVICE_URL);
        let userNumberElem2 = await driver.findElement(By.className("highlightBox"));
        let userNumber2 = await userNumberElem2.getText();
        // replace the user number for a reproducible screenshot
        await driver.executeScript("arguments[0].innerText = arguments[1];", userNumberElem2, '12345');
        expect(userNumber2).toBe(userNumber);
        await screenshot('05-welcome-back', driver);

        await driver.findElement(By.id('login')).click();
        // wait for device list to load
        await driver.wait(until.elementLocated(By.xpath(`//span[string()='${DEVICE_NAME1}']`)), 3_000);
        // should be logged in again, no point taking screenshot

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
        await driver.quit();
    }
};

async function registerNewIdentity(driver: ThenableWebDriver): Promise<string> {
    await driver.wait(until.elementLocated(By.id('registerButton')), 5_000).click();
    await driver.findElement(By.id('registerAlias')).sendKeys('Virtual WebAuthn device', Key.RETURN);
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
    await driver.findElement(By.id('registerUserNumber')).sendKeys(userNumber, Key.RETURN);
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
    while (await driver.executeScript("return document.fonts.status;") == "loading") {
      driver.sleep(500);
    }
}
