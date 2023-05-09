import { resolve } from "path";
import { UserConfig } from "vite";
import { configDefaults, defineConfig } from "vitest/config";

export default defineConfig(({ mode }: UserConfig): UserConfig => {
  return {
    resolve: {
      alias: {
        // Polyfill stream for the browser. e.g. needed in "Recovery Phrase" features.
        stream: "stream-browserify",
        // Custom alias we are using to shorten and make absolute the imports
        $generated: resolve(__dirname, "src/frontend/generated"),
        $src: resolve(__dirname, "src/frontend/src"),
      },
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
  };
});
