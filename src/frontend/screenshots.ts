#!/usr/bin/env node

import { isNullish } from "@dfinity/utils";
import { ChromeOptions } from "@wdio/types/build/Capabilities";
import { existsSync, mkdirSync } from "fs";
import { remote } from "webdriverio";

const SCREENSHOTS_DIR =
  process.env["SCREENSHOTS_DIR"] ?? "./screenshots/custom";

/** This executable takes screenshots of every page in the showcase.
 * This function expects the showcase to be running on 'http://localhost:5174'. Everything
 * else is automated. */
async function main() {
  // Set up the directory where we'll save the screenshots
  if (!existsSync(SCREENSHOTS_DIR)) {
    mkdirSync(SCREENSHOTS_DIR, { recursive: true });
  }

  await withChrome(async (browser) => {
    await takeLandingScreenshots(browser);
    await takeShowcaseScreenshots(browser);
  });
}

/** Take multiple screenshots spanning the entire landing page */
async function takeLandingScreenshots(browser: WebdriverIO.Browser) {
  // Use "authorizeNewKnown" as a good variation of the landing page
  await visit(browser, "http://localhost:5174/authorizeNewKnown");

  // There is no support for full page screenshots, so we take N screenshots
  // of the viewport, and scroll to the viewport height in between each screenshot
  const [screenshotHeight, totalHeight] = await browser.execute(() => {
    return [window.innerHeight, document.body.scrollHeight];
  });
  const nScreenshots = Math.ceil(totalHeight / screenshotHeight);

  for (let i = 0; i < nScreenshots; i++) {
    await browser.execute(
      (i, screenshotHeight) => {
        window.scrollTo(0, i * screenshotHeight);
      },
      i,
      screenshotHeight
    );
    await browser.saveScreenshot(`${SCREENSHOTS_DIR}/landing_${i + 1}.png`);
  }
}

/** Open each showcase page one after the other and screenshot it */
async function takeShowcaseScreenshots(browser: WebdriverIO.Browser) {
  await visit(browser, "http://localhost:5174/");

  // The landing page has a link for every page. The link tags have `data-page-name`
  // attributes, which we gather as the list of page names.
  const pageLinks = await browser.$$("[data-page-name]");
  const pageNames = await Promise.all(
    pageLinks.map(async (link) => {
      const pageName = await link.getAttribute("data-page-name");
      return pageName;
    })
  );

  // Iterate the pages and screenshot them
  for (const pageName of pageNames) {
    // Skip the loader, because it's animated
    if (pageName === "loader") {
      continue;
    }

    await visit(browser, `http://localhost:5174/${pageName}`);

    await browser.execute('document.body.style.caretColor = "transparent"');
    await browser.saveScreenshot(`${SCREENSHOTS_DIR}/${pageName}.png`);

    // When a chasm is present, toggle it
    if (await browser.$('[data-action="toggle-chasm"]').isExisting()) {
      await browser.$('[data-action="toggle-chasm"]').click();
      // Ensure the button is not hovered anymore for screenshot stability
      await browser
        .$('[data-action="toggle-chasm"]')
        .moveTo({ xOffset: -10, yOffset: -10 });
      await browser.saveScreenshot(`${SCREENSHOTS_DIR}/${pageName}_open.png`);
    }
  }
}

/** Create a chrome instance and run callback, deleting session afterwards */
async function withChrome<T>(
  cb: (browser: WebdriverIO.Browser) => T
): Promise<T> {
  // Screenshot image dimension, if specified
  const { mobileEmulation } = readScreenshotsConfig();

  const chromeOptions: ChromeOptions = {
    // font-render-hinting causing broken kerning on headless chrome seems to be a
    // long-standing issue: see https://github.com/puppeteer/puppeteer/issues/2410
    // -> disabling it improves things
    args: [
      "headless",
      "disable-gpu",
      "font-render-hinting=none",
      "hide-scrollbars",
    ],
    mobileEmulation,
  };

  const browser = await remote({
    capabilities: {
      browserName: "chrome",
      browserVersion: "119.0.6045.105",
      "goog:chromeOptions": chromeOptions,
    },
    automationProtocol: "webdriver",
  });

  if (isNullish(mobileEmulation)) {
    await browser.setWindowSize(1200, 900);
  }

  const res = await cb(browser);
  await browser.deleteSession();
  return res;
}

/** Visit page and wait until loaded */
async function visit(browser: WebdriverIO.Browser, url: string) {
  await browser.url(url);

  /* Disable transitions and animations to make sure we screenshot the (final) actual state */
  await browser.execute(() => {
    const notransition = `
*, *::before, *::after {
    -o-transition-property: none !important;
    -moz-transition-property: none !important;
    -ms-transition-property: none !important;
    -webkit-transition-property: none !important;
    transition-property: none !important;
}
        `;

    const noanimation = `
*, *::before, *::after {
    animation: none !important;
}
        `;
    const style = document.createElement("style");
    style.appendChild(document.createTextNode(notransition));
    style.appendChild(document.createTextNode(noanimation));
    document.body.appendChild(style);
  });

  /* Make sure lazy images are loaded eagerly */
  await browser.execute(() => {
    const imgs = Array.from(document.querySelectorAll("img"));
    imgs.forEach((img) => {
      if (img.getAttribute("loading") === "lazy") {
        img.setAttribute("loading", "eager");
      }
    });
  });

  /* Make sure everything has loaded */
  await browser.waitUntil(
    () =>
      browser.execute(() => {
        const imgs = Array.from(document.querySelectorAll("img"));
        return imgs.every((img) => img.complete);
      }),
    {
      timeout: 10 * 1000,
      timeoutMsg: "Images did not load after 10 seconds",
    }
  );

  await browser.waitUntil(
    () => browser.execute(() => document.readyState === "complete"),
    {
      timeout: 10 * 1000,
      timeoutMsg: "Browser did not load after 10 seconds",
    }
  );
}

/**
 * Read the screenshots configuration based on 'SCREENSHOTS_TYPE'
 * (either 'mobile' or 'desktop') and returns the appropriate device
 * name and/or window size.
 *
 * NOTE: deviceMetrics are necessary due to a bug in webdriverio
 * (otherwise just use 'deviceName'):
 * * https://github.com/webdriverio/webdriverio/issues/8903
 */
function readScreenshotsConfig(): {
  mobileEmulation?: {
    deviceMetrics: {
      width: number;
      height: number;
      pixelRatio: number;
      touch: boolean;
    };
  };
} {
  const screenshotsType = process.env["SCREENSHOTS_TYPE"];
  switch (screenshotsType) {
    case "mobile":
      return {
        mobileEmulation: {
          // Emulate a small modern device
          deviceMetrics: {
            width: 360,
            height: 640,
            pixelRatio: 1,
            touch: true,
          },
        },
      };
      break;
    case undefined:
      return {};
      break;
    case "desktop":
      return {};
      break;
    default:
      throw Error("Unknown screenshots type: " + screenshotsType);
      break;
  }
}

await main();
