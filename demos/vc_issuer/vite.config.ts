import { defineConfig } from "vite";
import { getReplicaHost } from "../../utils";
import { injectCanisterIdPlugin } from "../../vite.plugins";

export default defineConfig({
  root: ".",
  build: {
    rollupOptions: {
      input: ["index.html"],
      /* The issuer canister needs stable names */
      output: {
        entryFileNames: `[name].js`,
        chunkFileNames: `[name].js`,
        assetFileNames: `[name].[ext]`,
      },
    },
  },
  plugins: [injectCanisterIdPlugin({ canisterName: "issuer" })],
  optimizeDeps: {
    esbuildOptions: {
      define: {
        global: "globalThis",
      },
    },
  },
  server: {
    port: 5175,
    proxy: {
      "/api": getReplicaHost(),
    },
  },
});
