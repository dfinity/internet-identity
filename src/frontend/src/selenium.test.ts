import { Builder, By, until, ThenableWebDriver, Key} from 'selenium-webdriver';
import { Executor, Command } from 'selenium-webdriver/lib/command';
import { Options as ChromeOptions } from 'selenium-webdriver/chrome';

const IDP_SERVICE_URL = 'http://localhost:8000/?canisterId=rrkah-fqaaa-aaaaa-aaaaq-cai';
const OPEN_CHAT_URL = 'http://renrk-eyaaa-aaaaa-aaada-cai.localhost/';

test('Register new identity and login with it', async () => {
    await run_in_browser_with_virtual_authenticator(async (driver) => {
        await driver.get(IDP_SERVICE_URL);
        let userNumber = await registerNewIdentity(driver);
        await logout(driver);
        await login(userNumber, driver);
    })
}, 300_000);

test('Log in to OpenChat with OAUTH after registration', async () => {
    await run_in_browser_with_virtual_authenticator(async (driver) => {
        await driver.get(OPEN_CHAT_URL);
        await driver.wait(until.elementLocated(By.xpath("//button[string()='Sign-in']")), 3_000).click();

        let userNumber = await registerNewIdentity(driver);
        await driver.findElement(By.xpath("//button[text()='Yes']")).click();

        await driver.wait(until.urlIs(OPEN_CHAT_URL));
        let openChatUsername = "OpenChatter" + userNumber;
        await driver.wait(until.elementLocated(By.xpath('//*[@placeholder="Enter username"]')), 3_000).sendKeys(openChatUsername, Key.RETURN);
        await driver.wait(until.elementLocated(By.xpath("//h6[text()[contains(.,'"+openChatUsername+"')]]")), 20_000);
    })
}, 300_000);

async function run_in_browser_with_virtual_authenticator(test) {
    const driver = new Builder().forBrowser('chrome')
        .setChromeOptions(new ChromeOptions()
        .headless() // hides the click show: uncomment to watch it
        .windowSize({width: 1024, height: 768}))
        .build();
    try {
        const session = await driver.getSession();
        await addVirtualAuthenticator(driver.getExecutor(), session.getId());
        await test(driver);
    } finally {
        await driver.quit();
    }
};

async function registerNewIdentity(driver: ThenableWebDriver): Promise<string> {
    await driver.wait(until.elementLocated(By.id('registerButton')), 3_000).click();
    await driver.findElement(By.id('registerAlias')).sendKeys('Virtual WebAuthn device', Key.RETURN);
    let continueButton = await driver.wait(until.elementLocated(By.id('displayUserContinue')), 50_000);
    let userId = await driver.findElement(By.className("userNumberBox")).getText();
    await continueButton.click();
    return userId.replace("User ID:\n", "");
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
