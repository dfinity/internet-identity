import {
  compression,
  injectCanisterIdPlugin,
  inlineScriptsPlugin,
  minifyHTML,
  replicaForwardPlugin,
} from "@dfinity/internet-identity-vite-plugins";
import { readReplicaPort } from "@dfinity/internet-identity-vite-plugins/utils";
import basicSsl from "@vitejs/plugin-basic-ssl";
import { resolve } from "path";
import { AliasOptions, UserConfig, defineConfig } from "vite";

export const aliasConfig: AliasOptions = {
  // Polyfill stream for the browser. e.g. needed in "Recovery Phrase" features.
  stream: "stream-browserify",
  // Custom alias we are using to shorten and make absolute the imports
  $generated: resolve(__dirname, "src/frontend/generated"),
  $src: resolve(__dirname, "src/frontend/src"),
  $showcase: resolve(__dirname, "src/showcase/src"),
};

export default defineConfig(({ command, mode }): UserConfig => {
  // Expand process.env variables with default values to ensure build reproducibility
  process.env = {
    ...process.env,
    II_FETCH_ROOT_KEY: `${process.env.II_FETCH_ROOT_KEY ?? "0"}`,
    II_DUMMY_AUTH: `${process.env.II_DUMMY_AUTH ?? "0"}`,
    II_DUMMY_CAPTCHA: `${process.env.II_DUMMY_CAPTCHA ?? "0"}`,
    II_VERSION: `${process.env.II_VERSION ?? ""}`,
  };

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
        input: [
          "src/frontend/index.html",
          "src/frontend/faq.html",
          "src/frontend/vc-flow/index.html",
        ],
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
      inlineScriptsPlugin,
      [
        ...(mode === "development"
          ? [injectCanisterIdPlugin({ canisterName: "internet_identity" })]
          : []),
      ],
      [...(mode === "production" ? [minifyHTML(), compression()] : [])],
      [...(process.env.TLS_DEV_SERVER === "1" ? [basicSsl()] : [])],
      {
        ...replicaForwardPlugin({
          forwardDomains: ["icp0.io", "ic0.app"],
          forwardRules: [
            {
              hosts: ["nice-name.com"],
              canisterName: "test_app",
            },
            {
              hosts: ["nice-issuer-custom-orig.com"],
              canisterName: "issuer",
            },
            ...(process.env.NO_HOT_RELOAD === "1"
              ? [
                  {
                    hosts: [
                      "identity.ic0.app",
                      "identity.internetcomputer.org",
                    ],
                    canisterName: "internet_identity",
                  },
                ]
              : []),
          ],
        }),
        apply: "serve",
      },
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
        ? {}
        : {
            https: process.env.TLS_DEV_SERVER === "1",
            proxy: {
              "/api": `http://127.0.0.1:${readReplicaPort()}`,
            },
          },
  };
});
