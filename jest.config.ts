// eslint-disable-next-line
const path = require("path");
const internet_identity = path.join(
  __dirname,
  "src/frontend/generated/internet_identity_idl.ts"
);

export default {
  preset: "ts-jest/presets/js-with-ts-esm",
  testEnvironment: "jsdom",
  moduleNameMapper: {
    "dfx-generated/internet_identity": internet_identity,
  },
  setupFiles: [`<rootDir>/src/frontend/test-setup.ts`],

  testSequencer: "./src/frontend/jest-custom-sequencer.ts",

  // These two transform options make sure that jest can process files that include ES modules
  // (in particular, files that have lit-html import)
  transform: {
    // '^.+\\.[tj]sx?$' to process js/ts with `ts-jest`
    // '^.+\\.m?[tj]sx?$' to process js/ts/mjs/mts with `ts-jest`
    "^.+\\.[tj]sx?$": [
      "ts-jest",
      {
        useESM: true,
      },
    ],
  },
  transformIgnorePatterns: ["node_modules/(?!(@?lit|devtools|uuid))"],
};
