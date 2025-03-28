import { defineConfig, type UserConfig } from "vite";
import { readCanisterIds } from "./vite.plugins";

export default defineConfig(
  ({ mode }: UserConfig): UserConfig => ({
    root: "webapp",
    build: {
      outDir: "../dist",
      emptyOutDir: true,
      commonjsOptions: {
        // Source: https://github.com/rollup/plugins/issues/1425#issuecomment-1465626736
        strictRequires: true,
      },
    },
    define: {
      "process.env": {
        ...readCanisterIds(),
      },
    },
    optimizeDeps: {
      esbuildOptions: {
        define: {
          global: "globalThis",
        },
      },
    },
    server: {
      proxy: {
        "/api": "http://127.0.0.1:4943",
      },
    },
  })
);
