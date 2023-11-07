import { nonNullish } from "@dfinity/utils";
import basicSsl from "@vitejs/plugin-basic-ssl";
import { existsSync } from "fs";
import { resolve } from "path";
import { AliasOptions, UserConfig, defineConfig } from "vite";
import {
  compression,
  injectCanisterIdPlugin,
  minifyHTML,
  readCanisterId,
  replicaForwardPlugin,
} from "./vite.plugins";

export const aliasConfig: AliasOptions = {
  // Polyfill stream for the browser. e.g. needed in "Recovery Phrase" features.
  stream: "stream-browserify",
  // Custom alias we are using to shorten and make absolute the imports
  $generated: resolve(__dirname, "src/frontend/generated"),
  $src: resolve(__dirname, "src/frontend/src"),
  $showcase: resolve(__dirname, "src/showcase/src"),
};

export default defineConfig(({ mode }: UserConfig): UserConfig => {
  // Expand process.env variables with default values to ensure build reproducibility
  process.env = {
    ...process.env,
    II_FETCH_ROOT_KEY: `${process.env.II_FETCH_ROOT_KEY ?? "0"}`,
    II_DUMMY_AUTH: `${process.env.II_DUMMY_AUTH ?? "0"}`,
    II_DUMMY_CAPTCHA: `${process.env.II_DUMMY_CAPTCHA ?? "0"}`,
    II_VERSION: `${process.env.II_VERSION ?? ""}`,
  };

  const testAppCanisterId = existsSync(
    "demos/test-app/.dfx/local/canister_ids.json"
  )
    ? readCanisterId({
        canisterName: "test_app",
        canisterIdsJsonFile: "demos/test-app/.dfx/local/canister_ids.json",
      })
    : undefined;

  // Path "../../" have to be expressed relative to the "root".
  // e.g.
  // root = src/frontend
  // outDir = ../../dist
  return {
    root: "src/frontend",
    publicDir: "assets",
    envPrefix: "II_",
    resolve: {
      alias: aliasConfig,
    },
    build: {
      assetsInlineLimit: 0,
      outDir: "../../dist",
      emptyOutDir: true,
      rollupOptions: {
        // Bundle only english words in bip39.
        external: /.*\/wordlists\/(?!english).*\.json/,
        input: ["src/frontend/index.html", "src/frontend/faq.html"],
        output: {
          entryFileNames: `[name].js`,
          // II canister only supports resources that contains a single dot in their filenames. qr-creator.js.gz = ok. qr-creator.min.js.gz not ok. qr-creator.es6.min.js.gz no ok.
          chunkFileNames: ({ name }) => `${name.replace(/.es6|.min/gm, "")}.js`,
          assetFileNames: `[name].[ext]`,
        },
      },
      commonjsOptions: {
        // Source: https://github.com/rollup/plugins/issues/1425#issuecomment-1465626736
        strictRequires: true,
      },
    },
    plugins: [
      [...(mode === "development" ? [injectCanisterIdPlugin()] : [])],
      [...(mode === "production" ? [minifyHTML(), compression()] : [])],
      [...(process.env.TLS_DEV_SERVER === "1" ? [basicSsl()] : [])],
      replicaForwardPlugin({
        replicaOrigin: "127.0.0.1:4943",
        forwardRules: [
          ...(nonNullish(testAppCanisterId)
            ? [
                {
                  hosts: [
                    "nice-name.com",
                    `${testAppCanisterId}.ic0.app`,
                    `${testAppCanisterId}.icp0.io`,
                  ],
                  canisterId: testAppCanisterId,
                },
              ]
            : []),
          ...(process.env.NO_HOT_RELOAD === "1"
            ? [
                {
                  hosts: ["identity.ic0.app", "identity.internetcomputer.org"],
                  canisterId: readCanisterId({
                    canisterName: "internet_identity",
                    canisterIdsJsonFile: ".dfx/local/canister_ids.json",
                  }),
                },
              ]
            : []),
        ],
      }),
    ],
    optimizeDeps: {
      esbuildOptions: {
        define: {
          global: "globalThis",
        },
      },
    },
    server: {
      https: process.env.TLS_DEV_SERVER === "1",
      proxy: {
        "/api": "http://127.0.0.1:4943",
      },
    },
  };
});
