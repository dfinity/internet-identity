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
    },
  })
);
