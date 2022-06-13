import { remote } from "webdriverio";
import { command } from "webdriver";
import { WebAuthnCredential } from "../../test-setup";

// mobile resolution is used when env variable SCREEN=mobile is set
const MOBILE_SCREEN: ScreenConfiguration = {
  screenType: "mobile",
  windowSize: "360,640",
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
  await runInBrowserCommon(true, test);
}

export async function runInNestedBrowser(
  test: (
    browser: WebdriverIO.Browser,
    runConfig: RunConfiguration
  ) => Promise<void>
): Promise<void> {
  await runInBrowserCommon(false, test);
}

export async function runInBrowserCommon(
  outer: boolean,
  test: (
    browser: WebdriverIO.Browser,
    runConfig: RunConfiguration
  ) => Promise<void>
): Promise<void> {
  // parse run configuration from environment variables
  const runConfig = parseRunConfiguration();

  const browser = await remote({
    capabilities: {
      browserName: "chrome",
      "goog:chromeOptions": {
        args: [
          "--headless",
          "--ignore-certificate-errors", // allow self-signed certificates
          "--disable-gpu",
          `--window-size=${runConfig.screenConfiguration.windowSize}`,
        ],
      },
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
    console.log(await browser.getPageSource());
    console.error(e);
    await browser.saveScreenshot(
      `screenshots/error/${new Date().getTime()}.png`
    );
    console.log(
      "An error occurred during e2e test execution. Logs can be found in the wdio.log file and an additional error screenshot was saved under screenshots/error. On Github Actions you can find the log and screenshots under 'Artifacts'."
    );
    throw e;
  } finally {
    if (outer) {
      // only close outer session
      await browser.deleteSession();
    }
  }
}

export interface ScreenConfiguration {
  screenType: "desktop" | "mobile";
  windowSize: string;
}

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

// 'Screenshots' objects are used to make sure all screenshots end up in the
// same directory, each with a different (increasing) number prefixed in the
// filename.
export class Screenshots {
  private count = 0;

  constructor(private directory: string, private suffix: string) {}

  async take(name: string, browser: WebdriverIO.Browser): Promise<void> {
    await waitForImages(browser);
    await waitForFonts(browser);
    // Make sure that all screenshots are prefixed with "01-", "02-", ...
    const countStr: string = this.count.toFixed().padStart(2, "0");
    this.count++;
    await browser.saveScreenshot(
      `${this.directory}/${countStr}-${name}-${this.suffix}.png`
    );
  }
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
  const handles = await browser.getWindowHandles();
  expect(handles.length).toBe(2);
  await browser.switchToWindow(handles[1]);
  // enable virtual authenticator in the new window
  return await addVirtualAuthenticator(browser);
}

export async function removeFeaturesWarning(
  browser: WebdriverIO.Browser
): Promise<void> {
  const warningContainer = await browser.$(".features-warning-container");
  await warningContainer.waitForDisplayed();
  await browser.execute(() => {
    const warningContainer = document.querySelector(
      ".features-warning-container"
    );
    if (warningContainer) {
      warningContainer.remove();
    }
  });
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
