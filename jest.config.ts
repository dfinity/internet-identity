// eslint-disable-next-line
const path = require("path");
const internet_identity = path.join(
  __dirname,
  "src/frontend/generated/internet_identity_idl.ts"
);

export default {
  preset: "ts-jest",
  testEnvironment: "jsdom",
  moduleNameMapper: {
    "dfx-generated/internet_identity": internet_identity,
    "^\\$generated/(.*)$": "<rootDir>/src/frontend/generated/$1",
  },
  setupFiles: [`<rootDir>/src/frontend/test-setup.ts`],

  testSequencer: "./src/frontend/jest-custom-sequencer.ts",

  // These two transform options make sure that jest can process files that include ES modules
  // (in particular, files that have lit-html import)
  transform: {
    "\\.[jt]sx?$": "ts-jest",
    "\\.(svg|png|webp)$": "<rootDir>/jest-transform.cjs",
  },
  transformIgnorePatterns: ["node_modules/(?!@?lit)"],
};
