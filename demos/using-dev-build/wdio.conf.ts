export const config: WebdriverIO.Config = {
  baseUrl: process.env.II_DAPP_URL || "http://localhost:5173",

  waitforTimeout: 10_000,

  // Add retry mechanism
  connectionRetryTimeout: 120000,
  connectionRetryCount: 3,

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
      browserVersion: "133.0.6943.53", // More information about available versions can be found here: https://github.com/GoogleChromeLabs/chrome-for-testing
      "goog:chromeOptions": {
        args: [
          "headless",
          "disable-gpu",
          "disable-dev-shm-usage",
          // Required for CI runners using >=Ubuntu 24.04
          // @see https://github.com/SeleniumHQ/selenium/issues/14609
          "--no-sandbox",
        ],
      },
      acceptInsecureCerts: true,
    },
  ],
  // Add before hook for cleanup
  beforeSession: function () {
    // Clean up any existing Chrome processes
    if (process.platform !== "win32") {
      try {
        require("child_process").execSync("pkill -f chrome", {
          stdio: "ignore",
        });
      } catch (e) {
        // Ignore errors if no chrome processes exist
      }
    }
  },
  logLevel: "info",

  framework: "mocha",
  reporters: ["spec"],

  mochaOpts: {
    ui: "bdd",
    timeout: 60000,
  },
};
