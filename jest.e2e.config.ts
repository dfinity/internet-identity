export default {
  extensionsToTreatAsEsm: [".ts"],
  moduleNameMapper: {
    "^\\$generated/(.*)$": "<rootDir>/src/frontend/generated/$1",
    "^\\$root/(.*)$": "<rootDir>/src/frontend/src/$1",
  },
  transform: {
    // '^.+\\.[tj]sx?$' to process js/ts with `ts-jest`
    // '^.+\\.m?[tj]sx?$' to process js/ts/mjs/mts with `ts-jest`
    "^.+\\.tsx?$": [
      "ts-jest",
      {
        useESM: true,
      },
    ],
  },
};
