const path = require("path");
const idp_service = path.join(
  __dirname,
  ".dfx/local/canisters/idp_service/idp_service.js"
);

export default {
  preset: "ts-jest",
  testEnvironment: "jsdom",
  moduleNameMapper: {
    "dfx-generated/idp_service": idp_service,
  },
  transform: {
    "^.+\\.jsx?$": "ts-jest",
    "^.+\\.tsx?$": "ts-jest",
  },
  setupFiles: ["./setupTests.ts"],
};
