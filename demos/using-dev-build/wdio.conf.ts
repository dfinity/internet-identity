export const config: WebdriverIO.Config = {
  baseUrl: process.env.II_DAPP_URL || "http://localhost:5173",

  waitforTimeout: 60_000,

  // Add retry mechanism
  connectionRetryTimeout: 180000,
  connectionRetryCount: 5,

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
          "--disable-setuid-sandbox",
          "--window-size=1920,1080",
          "--disable-extensions",
          "--disable-background-networking",
          "--disable-background-timer-throttling",
          "--disable-backgrounding-occluded-windows",
          "--disable-client-side-phishing-detection",
          "--disable-default-apps",
          "--disable-hang-monitor",
          "--disable-prompt-on-repost",
          "--disable-sync",
          "--metrics-recording-only",
          "--no-first-run",
          "--password-store=basic",
          "--use-mock-keychain",
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
  // Add better error handling
  onPrepare: function () {
    // Clean temp files that might cause ETXTBSY
    if (process.platform !== "win32") {
      try {
        require("child_process").execSync(
          "rm -rf /tmp/.org.chromium.Chromium*",
          { stdio: "ignore" }
        );
        require("child_process").execSync("rm -rf /tmp/.com.google.Chrome*", {
          stdio: "ignore",
        });
      } catch (e) {
        // Ignore cleanup errors
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
