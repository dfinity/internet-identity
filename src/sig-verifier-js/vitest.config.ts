import { UserConfig } from "vite";
import wasm from "vite-plugin-wasm";
import { defineConfig } from "vitest/config";

export default defineConfig(
  ({ mode }: UserConfig): UserConfig => ({
    plugins: [wasm()],
    test: {
      environment: "node",
      include: "./tests.ts",
      globals: true /* globals like 'test' and 'describe' */,
      watch: false,
    },
  }),
);
