// eslint-disable-next-line
const path = require("path");
const internet_identity = path.join(__dirname, "src/frontend/generated/internet_identity_idl.ts");

export default {
  preset: "ts-jest",
  testEnvironment: "jsdom",
  moduleNameMapper: {
    "dfx-generated/internet_identity": internet_identity,
  },
  transform: {
    "^.+\\.jsx?$": "ts-jest",
    "^.+\\.tsx?$": "ts-jest",
  },
};
