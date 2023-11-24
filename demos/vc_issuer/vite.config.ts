import { defineConfig } from "vite";
import { getReplicaHost } from "../../utils";
import { injectCanisterIdPlugin } from "../../vite.plugins";

export default defineConfig({
  root: ".",
  build: {
    rollupOptions: {
      input: ["index.html"],
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
