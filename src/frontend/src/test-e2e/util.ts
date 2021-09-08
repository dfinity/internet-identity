import { Builder, logging, ThenableWebDriver } from "selenium-webdriver";
import { Command } from "selenium-webdriver/lib/command";
import { writeFile } from "fs/promises";
import { Options as ChromeOptions } from "selenium-webdriver/chrome";

export async function addVirtualAuthenticator(
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

export async function removeVirtualAuthenticator(
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

export async function screenshot(name: string, driver: ThenableWebDriver) {
  const image = await driver.takeScreenshot();
  // writing to a subdirectory has the nice property that it fails if
  // this is run in the wrong directory
  await writeFile(`screenshots/${name}.png`, image, "base64");
}

// Inspired by https://stackoverflow.com/a/66919695/946226
export async function waitForFonts(driver: ThenableWebDriver) {
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

export async function runInBrowser(
  test: (driver: ThenableWebDriver) => Promise<void>
) {
  await runInBrowserCommon(true, test);
}

export async function runInNestedBrowser(
  test: (driver: ThenableWebDriver) => Promise<void>
) {
  await runInBrowserCommon(false, test);
}

export async function runInBrowserCommon(
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

export async function switchToPopup(driver: ThenableWebDriver) {
  let handles = await driver.getAllWindowHandles();
  expect(handles.length).toBe(2);
  await driver.switchTo().window(handles[1]);
  // enable virtual authenticator in the new window
  await addVirtualAuthenticator(driver);
}

export async function waitToClose(driver: ThenableWebDriver) {
  await driver.wait(async (driver: ThenableWebDriver) => {
    return (await driver.getAllWindowHandles()).length == 1;
  }, 10_000);

  const handles = await driver.getAllWindowHandles();
  expect(handles.length).toBe(1);
  await driver.switchTo().window(handles[0]);
}
