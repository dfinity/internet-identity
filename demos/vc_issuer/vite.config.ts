import {
  getReplicaHost,
  injectCanisterIdPlugin,
} from "@dfinity/internet-identity-vite-plugins";
import { defineConfig } from "vite";
import { nodePolyfills } from "vite-plugin-node-polyfills";

export default defineConfig(({ command, mode }) => ({
  root: "./app",
  build: {
    outDir: "../dist" /* relative to 'root' */,
    emptyOutDir: true /* needed because 'outDir' is outside of 'root' */,
    rollupOptions: {
      input: ["./app/index.html"],
      /* The issuer canister needs stable names */
      output: {
        entryFileNames: `[name].js`,
        chunkFileNames: `[name].js`,
        assetFileNames: `[name].[ext]`,
      },
    },
  },
  plugins: [
    nodePolyfills({ include: ["buffer"] }),
    ...(mode === "development"
      ? [injectCanisterIdPlugin({ canisterName: "issuer" })]
      : []),
  ],
  optimizeDeps: {
    esbuildOptions: {
      define: {
        global: "globalThis",
      },
    },
  },
  server:
    command !== "serve"
      ? undefined
      : {
          port: 5175,
          proxy: {
            "/api": getReplicaHost(),
          },
        },
}));
