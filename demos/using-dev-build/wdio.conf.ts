import { existsSync, mkdirSync } from "fs";

export const config: WebdriverIO.Config = {
  baseUrl: process.env.II_DAPP_URL || "http://localhost:8080",

  waitforTimeout: 10_000,

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
