import { UserConfig, defineConfig } from "vite";
import { getReplicaHost } from "../../utils";
import { injectCanisterIdPlugin } from "../../vite.plugins";

export default defineConfig(({mode}) => ({
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
  plugins: [
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
  server: {
    port: 5175,
    proxy: {
      "/api": getReplicaHost(),
    },
  },
}));
