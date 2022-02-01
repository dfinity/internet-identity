import { existsSync, mkdirSync } from "fs";

console.log(process.env);

export const config: WebdriverIO.Config = {
  baseUrl: process.env.II_DAPP_URL || "http://localhost:8080",

  before: (capabilities, spec) => {
    browser["screenshot-count"] = 0;
    browser["screenshots-taken"] = new Set();

    browser.addCommand("screenshot", async (name) => {
      const countStr: string = browser["screenshots-taken"].size
        .toFixed()
        .padStart(2, "0");
      if (browser["screenshots-taken"].has(name)) {
        throw Error(`A screenshot with this name was already taken: '${name}'`);
      }
      browser["screenshots-taken"].add(name);

      const SCREENSHOTS_DIR = "screenshots";
      if (!existsSync(SCREENSHOTS_DIR)) {
        mkdirSync(SCREENSHOTS_DIR);
      }

      await browser.saveScreenshot(
        `${SCREENSHOTS_DIR}/${countStr}-${name}.png`
      );
    });
  },

  autoCompileOpts: {
    autoCompile: true,
    tsNodeOpts: {
      transpileOnly: true,
      project: "tsconfig.json",
    },
  },
  specs: ["./specs/**/*.ts"],
  exclude: [],
  capabilities: [
    {
      browserName: "chrome",
      "goog:chromeOptions": {
        args: ["headless", "disable-gpu"],
      },
      acceptInsecureCerts: true,
    },
  ],
  logLevel: "info",
  services: ["chromedriver"],

  framework: "mocha",
  reporters: ["spec"],

  mochaOpts: {
    ui: "bdd",
    timeout: 60000,
  },
};
