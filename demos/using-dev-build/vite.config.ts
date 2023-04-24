import { defineConfig, type UserConfig } from "vite";
import { readCanisterIds } from "./vite.plugins";

// npm run dev = local
// npm run build = local
// dfx deploy = local
// dfx deploy --network ic = ic
const network = process.env.DFX_NETWORK ?? ("local" as const);

export default defineConfig(
  ({ mode }: UserConfig): UserConfig => ({
    root: "webapp",
    build: {
      outDir: "../dist",
      emptyOutDir: true,
    },
    define: {
      "process.env": {
        ...readCanisterIds({ network }),
        DFX_NETWORK: network,
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
      port: 8080,
      proxy: {
        "/api": "http://127.0.0.1:4943",
      },
    },
  })
);
