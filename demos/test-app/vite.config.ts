import {
  getReplicaHost,
  readCanisterId,
} from "@dfinity/internet-identity-vite-plugins";
import { defineConfig } from "vite";
import { nodePolyfills } from "vite-plugin-node-polyfills";

const rewriteRoute = (pathAndParams: string): string => {
  let queryParamsString = `?`;

  const [path, params] = pathAndParams.split("?");

  if (params) {
    queryParamsString += `${params}&`;
  }

  queryParamsString += `canisterId=${readCanisterId({
    canisterName: "test_app",
  })}`;

  return path + queryParamsString;
};

export default defineConfig(({ command, mode }) => ({
  root: "./src",
  build: {
    outDir: "../dist",
    emptyOutDir: true,
    rollupOptions: {
      output: {
        entryFileNames: `[name].js`,
        chunkFileNames: `[name].js`,
        assetFileNames: `[name].[ext]`,
      },
    },
  },
  optimizeDeps: {
    esbuildOptions: {
      define: {
        global: "globalThis",
      },
    },
  },
  plugins: [nodePolyfills({ include: ["buffer"] })],
  server:
    command !== "serve"
      ? undefined
      : {
          port: 8081,
          // Set up a proxy that redirects API calls and /index.html to the
          // replica; the rest we serve from here.
          proxy: {
            "/api": getReplicaHost(),
            "/.well-known/ii-alternative-origins": {
              target: getReplicaHost(),
              rewrite: rewriteRoute,
            },
            "/.well-known/evil-alternative-origins": {
              target: getReplicaHost(),
              rewrite: rewriteRoute,
            },
          },
        },
}));
