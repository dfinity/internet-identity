import { randomString, wrapError } from "$src/utils/utils";
import { ChromeOptions } from "@wdio/types/build/Capabilities";
import * as fs from "fs";
import * as fsasync from "fs/promises";
import { command } from "webdriver";
import { remote } from "webdriverio";
import { WebAuthnCredential } from "../../test-setup";

// mobile resolution is used when env variable SCREEN=mobile is set
const MOBILE_SCREEN: ScreenConfiguration = {
  screenType: "mobile",
  deviceMetrics: {
    width: 360,
    height: 640,
  },
};

// desktop resolution is used when env variable SCREEN=desktop is set
const DESKTOP_SCREEN: ScreenConfiguration = {
  screenType: "desktop",
  windowSize: "1920,1080",
};

export async function runInBrowser(
  test: (
    browser: WebdriverIO.Browser,
    runConfig: RunConfiguration
  ) => Promise<void>
): Promise<void> {
  // parse run configuration from environment variables
  const runConfig = parseRunConfiguration();

  const chromeOptions: ChromeOptions = {
    args: [
      "--ignore-certificate-errors", // allow self-signed certificates
      "--disable-gpu",
    ],

    // Disables permission prompt for clipboard, needed for tests using the clipboard (without this,
    // the browser window prompts the user to allow or block clipboard access).
    // https://stackoverflow.com/questions/53669639/enable-clipboard-in-automated-tests-with-protractor-and-webdriver
    // XXX: the "any" cast is needed because our webdriverio version only expects numbers, strings
    // or bools: https://github.com/webdriverio/webdriverio/issues/9924
    prefs: {
      "profile.content_settings.exceptions.clipboard": {
        "*": { last_modified: Date.now(), setting: 1 },
      } as any,
    },
  };

  if (runConfig.screenConfiguration.screenType === "mobile") {
    chromeOptions.mobileEmulation = {
      deviceMetrics: runConfig.screenConfiguration.deviceMetrics,
    };
  } else {
    chromeOptions.args?.push(
      `--window-size=${runConfig.screenConfiguration.windowSize}`
    );
  }

  const browser = await remote({
    capabilities: {
      browserName: "chrome",
      "goog:chromeOptions": chromeOptions,
    },
    automationProtocol: "webdriver",
    path: "/wd/hub",
    logLevel: "info",
    // outputDir pipes all webdriver log output into ./wdio.log
    // stdout only contains errors on test failures
    outputDir: "./",
  });

  // setup test suite
  await addCustomCommands(browser);

  try {
    // run test
    await test(browser, runConfig);
  } catch (e) {
    const testName =
      expect.getState().currentTestName?.replace(/\W/g, "_") ??
      `unknown-${randomString()}`;
    if (!fs.existsSync("test-failures")) {
      fs.mkdirSync("test-failures");
    }
    await fsasync.writeFile(
      `test-failures/${testName}.html`,
      await browser.getPageSource()
    );
    const browserLogs = await browser.getLogs("browser");
    const printableLogs = browserLogs.reduce(
      (accumulator, entry) => accumulator + "\n" + JSON.stringify(entry),
      ""
    );

    await fsasync.writeFile(`test-failures/${testName}.log`, printableLogs);
    await browser.saveScreenshot(`test-failures/${testName}.png`);
    console.error(e);
    console.log(
      "An error occurred during e2e test execution. WebDriver logs can be found in the wdio.log file and an error information (screenshot, console logs, page source) was saved in the test-failures folder. On Github Actions you can find the log and screenshots under 'Artifacts'."
    );
    throw e;
  } finally {
    try {
      await browser.deleteSession();
    } catch (e) {
      console.error("error occurred during session cleanup: " + wrapError(e));
    }
  }
}

export type ScreenConfiguration =
  | {
      screenType: "desktop";
      windowSize: string;
    }
  | {
      screenType: "mobile";
      deviceMetrics: {
        width?: number;
        height?: number;
        pixelRatio?: number;
        touch?: boolean;
      };
    };

export interface RunConfiguration {
  screenConfiguration: ScreenConfiguration;
}

function parseScreen(): ScreenConfiguration {
  switch (process.env.SCREEN) {
    case MOBILE_SCREEN.screenType:
      return MOBILE_SCREEN;
    case DESKTOP_SCREEN.screenType:
      return DESKTOP_SCREEN;
    default:
      console.log(
        `Using default screen 'desktop'. Unknown screen type provided by SCREEN env variable: '${process.env.SCREEN}'`
      );
      return DESKTOP_SCREEN;
  }
}

function parseRunConfiguration(): RunConfiguration {
  return {
    screenConfiguration: parseScreen(),
  };
}

/**
 * Adds custom commands for webauthn authenticator administration as documented here: https://webdriver.io/docs/customcommands/#add-more-webdriver-commands
 * @param browser browser to add the commands to
 */
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

  // This retrieves previously created credentials, see https://www.w3.org/TR/webauthn-2/#sctn-automation-get-credentials
  await browser.addCommand(
    "getWebauthnCredentials",
    command(
      "GET",
      "/session/:sessionId/webauthn/authenticator/:authenticatorId/credentials",
      {
        command: "getWebauthnCredentials",
        description: "retrieves the credentials of a virtual authenticator",
        ref: "https://www.w3.org/TR/webauthn-2/#sctn-automation-get-credentials",
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

  // This adds a previously created credential, see https://www.w3.org/TR/webauthn-2/#sctn-automation-add-credential
  await browser.addCommand(
    "addWebauthnCredential",
    command(
      "POST",
      "/session/:sessionId/webauthn/authenticator/:authenticatorId/credential",
      {
        command: "addWebauthnCredential",
        description: "Adds a credential to a virtual authenticator",
        ref: "https://www.w3.org/TR/webauthn-2/#sctn-automation-add-credential",
        variables: [
          {
            name: "authenticatorId",
            type: "string",
            description: "The id of the authenticator to remove",
            required: true,
          },
        ],
        parameters: [
          {
            name: "rpId",
            type: "string",
            description: "The relying party ID the credential is scoped to.",
            required: true,
          },
          {
            name: "credentialId",
            type: "string",
            description: "The credential ID encoded using Base64url encoding",
            required: true,
          },
          {
            name: "isResidentCredential",
            type: "boolean",
            description:
              "If set to true, a client-side discoverable credential is created. If set to false, a server-side credential is created instead.",
            required: true,
          },
          {
            name: "privateKey",
            type: "string",
            description:
              "An asymmetric key package containing a single private key per [RFC5958], encoded using Base64url encoding.",
            required: true,
          },
          {
            name: "signCount",
            type: "number",
            description:
              "The initial value for a signature counter associated to the public key credential source.",
            required: true,
          },
          {
            name: "userHandle",
            type: "string",
            description:
              "The userHandle associated to the credential encoded using Base64url encoding.",
            required: false,
          },
          {
            name: "largeBlob",
            type: "string",
            description:
              "The large, per-credential blob associated to the public key credential source, encoded using Base64url encoding.",
            required: false,
          },
        ],
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

export async function getWebAuthnCredentials(
  browser: WebdriverIO.Browser,
  authId: string
): Promise<WebAuthnCredential[]> {
  return await browser.getWebauthnCredentials(authId);
}

export async function addWebAuthnCredential(
  browser: WebdriverIO.Browser,
  authId: string,
  credential: WebAuthnCredential,
  rpId: string
): Promise<void> {
  return await browser.addWebauthnCredential(
    authId,
    rpId,
    credential.credentialId,
    credential.isResidentCredential,
    credential.privateKey,
    credential.signCount
  );
}

export function originToRelyingPartyId(origin: string): string {
  return origin.replace(/https?:\/\/([.\w]+).*/, "$1");
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

// Inspired by https://github.com/dfinity/nns-dapp/blob/0449da36fd20eb9bb5d712d78aea8879cb51ec8e/e2e-tests/common/waitForImages.ts
export const waitForImages = async (
  browser: WebdriverIO.Browser
): Promise<true | void> =>
  // Wait for all images to be "complete", i.e. loaded
  browser.waitUntil(
    () =>
      browser.execute(function () {
        const imgs: HTMLCollectionOf<HTMLImageElement> =
          document.getElementsByTagName("img");
        if (imgs.length <= 0) {
          return true;
        }

        const imagesReady: boolean = Array.prototype.every.call(imgs, (img) => {
          return img.complete;
        });
        const documentReady: boolean = document.readyState === "complete";
        return imagesReady && documentReady;
      }),
    { timeoutMsg: "image wasn't loaded" }
  );

export async function switchToPopup(
  browser: WebdriverIO.Browser
): Promise<string> {
  await browser.waitUntil(
    async () => (await browser.getWindowHandles()).length === 2,
    { timeoutMsg: "window did not open" }
  );
  const handles = await browser.getWindowHandles();
  await browser.switchToWindow(handles[1]);
  // enable virtual authenticator in the new window
  return await addVirtualAuthenticator(browser);
}

/**
 * Switches to the first window of the given browser. Explicit window focusing is required when using the mobile emulation mode of chrome.
 * @param browser to switch to.
 */
export async function focusBrowser(
  browser: WebdriverIO.Browser
): Promise<void> {
  await browser.switchToWindow((await browser.getWindowHandles())[0]);
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
