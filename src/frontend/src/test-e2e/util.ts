import { remote } from "webdriverio";
import { command } from "webdriver";
import * as SeleniumStandalone from "selenium-standalone";
import { ChildProcess } from "selenium-standalone";

export async function runInBrowser(
  test: (browser: WebdriverIO.Browser) => Promise<void>
): Promise<void> {
  await runInBrowserCommon(true, test);
}

export async function runInNestedBrowser(
  test: (browser: WebdriverIO.Browser) => Promise<void>
): Promise<void> {
  await runInBrowserCommon(false, test);
}

export async function startWebdriver(): Promise<ChildProcess | undefined> {
  let webdriverProcess: ChildProcess | undefined;
  let retryCount = 0;
  let error;
  while (!webdriverProcess && retryCount < 10) {
    try {
      error = undefined;
      webdriverProcess = await SeleniumStandalone.start();
    } catch (e) {
      // port may still be used from previous stopped webdriver, try again
      error = e;
      retryCount++;
      await new Promise((resolve) => setTimeout(resolve, 1000));
    }
  }
  if (error !== undefined) {
    console.warn(
      'selenium could not be started. Make sure you installed the required webdrivers ("install-webdrivers")'
    );
    console.error(error);
  }
  return webdriverProcess;
}

export async function runInBrowserCommon(
  outer: boolean,
  test: (browser: WebdriverIO.Browser) => Promise<void>
): Promise<void> {
  let webdriverProcess;
  if (outer) {
    webdriverProcess = await startWebdriver();
  }

  const browser = await remote({
    capabilities: {
      browserName: "chrome",
      "goog:chromeOptions": {
        args: ["--headless", "--disable-gpu", "--window-size=1050,1400"],
      },
    },
    automationProtocol: "webdriver",
    path: "/wd/hub",
    logLevel: "info",
    // outputDir pipes all webdriver log output into ./wdio.log
    outputDir: "./",
  });

  // setup test suite
  await addCustomCommands(browser);

  try {
    // run test
    await test(browser);
  } catch (e) {
    console.log(await browser.getPageSource());
    console.error(e);
    await browser.saveScreenshot(
      `screenshots/error/${new Date().toISOString()}.png`
    );
    console.log(
      "An error occurred during e2e test execution. Logs can be found in the wdio.log file and an additional error screenshot was saved under screenshots/error. On Github Actions you can find the log and screenshots under 'Artifacts'."
    );
    throw e;
  } finally {
    if (outer) {
      // only close outer session
      await browser.deleteSession();
      webdriverProcess?.kill();
    }
  }
}

export async function addCustomCommands(
  browser: WebdriverIO.Browser
): Promise<void> {
  await browser.addCommand(
    "addVirtualWebAuth",
    command("POST", "/session/:sessionId/webauthn/authenticator", {
      command: "addVirtualWebAuth",
      description: "add a virtual authenticator",
      ref: "https://www.w3.org/TR/webauthn-2/#sctn-automation-add-virtual-authenticator",
      variables: [],
      parameters: [
        {
          name: "protocol",
          type: "string",
          description: "The protocol the Virtual Authenticator speaks",
          required: true,
        },
        {
          name: "transport",
          type: "string",
          description: "The AuthenticatorTransport simulated",
          required: true,
        },
        {
          name: "hasResidentKey",
          type: "boolean",
          description:
            "If set to true the authenticator will support client-side discoverable credentials",
          required: true,
        },
        {
          name: "isUserConsenting",
          type: "boolean",
          description:
            "Determines the result of all user consent authorization gestures",
          required: true,
        },
      ],
    })
  );

  await browser.addCommand(
    "removeVirtualWebAuth",
    command(
      "DELETE",
      "/session/:sessionId/webauthn/authenticator/:authenticatorId",
      {
        command: "removeVirtualWebAuth",
        description: "remove a virtual authenticator",
        ref: "https://www.w3.org/TR/webauthn-2/#sctn-automation-add-virtual-authenticator",
        variables: [
          {
            name: "authenticatorId",
            type: "string",
            description: "The id of the authenticator to remove",
            required: true,
          },
        ],
        parameters: [],
      }
    )
  );
}

export async function addVirtualAuthenticator(
  browser: WebdriverIO.Browser
): Promise<string> {
  return await browser.addVirtualWebAuth("ctap2", "usb", true, true);
}

export async function removeVirtualAuthenticator(
  browser: WebdriverIO.Browser,
  authenticatorId: string
): Promise<void> {
  return await browser.removeVirtualWebAuth(authenticatorId);
}

export async function screenshot(
  name: string,
  browser: WebdriverIO.Browser
): Promise<void> {
  await browser.saveScreenshot(`screenshots/${name}.png`);
}

// Inspired by https://stackoverflow.com/a/66919695/946226
export async function waitForFonts(
  browser: WebdriverIO.Browser
): Promise<void> {
  for (let i = 0; i <= 50; i++) {
    if ((await browser.execute("return document.fonts.status;")) == "loaded") {
      return;
    }
    await browser.pause(200);
  }
  console.log(
    "Odd, document.font.status never reached state loaded, stuck at",
    await browser.execute("return document.fonts.status;")
  );
}

export async function switchToPopup(
  browser: WebdriverIO.Browser
): Promise<void> {
  const handles = await browser.getWindowHandles();
  expect(handles.length).toBe(2);
  await browser.switchToWindow(handles[1]);
  // enable virtual authenticator in the new window
  await addVirtualAuthenticator(browser);
}

export async function waitToClose(browser: WebdriverIO.Browser): Promise<void> {
  await browser.waitUntil(
    async () => (await browser.getWindowHandles()).length == 1,
    {
      timeout: 10_000,
      timeoutMsg: "expected only one window to exist after 10s",
    }
  );
  const handles = await browser.getWindowHandles();
  expect(handles.length).toBe(1);
  await browser.switchToWindow(handles[0]);
}
