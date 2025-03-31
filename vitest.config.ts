import { UserConfig } from "vite";
import { configDefaults, defineConfig } from "vitest/config";
import { aliasConfig } from "./vite.config";

export default defineConfig(
  ({ mode }: UserConfig): UserConfig => ({
    resolve: {
      alias: aliasConfig,
    },
    test: {
      environment: "jsdom",
      exclude: [
        ...configDefaults.exclude,
        ...(mode === "test" ? ["src/frontend/src/test-e2e/**"] : []),
      ],
      include: [
        ...configDefaults.include,
        ...(mode === "e2e" ? ["src/frontend/src/test-e2e/**/*.test.ts"] : []),
      ],
      globals: true,
      watch: false,
      setupFiles: "./src/frontend/test-setup.ts",
      // Make sure that our browser e2e tests run one by one:
      // - `sequence.concurrent: false` within each test file
      // - `fileParallelism: false` across test files
      //
      // This makes sure that WebdriverIO won't start more than one browser
      // instance at the same time, which would result in the following error:
      // `Could not start browser after 5 retries: Error: spawn ETXTBSY`
      sequence: {
        concurrent: false,
      },
      fileParallelism: false,
    },
  }),
);
