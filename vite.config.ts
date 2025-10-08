import { replicaForwardPlugin } from "@dfinity/internet-identity-vite-plugins";
import tailwindcss from "@tailwindcss/vite";
import { readReplicaPort } from "@dfinity/internet-identity-vite-plugins/utils";
import { sveltekit } from "@sveltejs/kit/vite";
import basicSsl from "@vitejs/plugin-basic-ssl";
import { type AliasOptions, type UserConfig, defineConfig } from "vite";
import { nodePolyfills } from "vite-plugin-node-polyfills";
import typescript from "rollup-plugin-typescript2";
import { transform } from "@formatjs/ts-transformer";
import { readFileSync } from "fs";
import path from "path";
import { writeFileSync } from "node:fs";

const localeFile = path.join(__dirname, "locales/en.json");

export const aliasConfig: AliasOptions = {
  // Polyfill stream for the browser. e.g. needed in "Recovery Phrase" features.
  stream: "stream-browserify",
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

  return {
    envPrefix: "II_",
    resolve: {
      alias: aliasConfig,
    },
    build: {
      assetsInlineLimit: 0,
      emptyOutDir: true,
      rollupOptions: {
        // Bundle only english words in bip39.
        external: /.*\/wordlists\/(?!english).*\.json/,
      },
      commonjsOptions: {
        // Source: https://github.com/rollup/plugins/issues/1425#issuecomment-1465626736
        strictRequires: true,
      },
    },
    plugins: [
      tailwindcss(),
      sveltekit(),
      ...(command === "build"
        ? [
            // See: https://formatjs.github.io/docs/guides/bundler-plugins
            {
              name: "reset-extracted-locale",
              buildStart() {
                // Reset extracted locale file before updating it below
                writeFileSync(localeFile, JSON.stringify({}));
              },
              buildEnd() {
                // Sort extracted locale file by keys
                const json = readFileSync(localeFile, "utf8").trim();
                const en = JSON.parse(json);
                const sorted = Object.fromEntries(
                  Object.entries(en).sort(([a], [b]) => a.localeCompare(b)),
                );
                writeFileSync(localeFile, JSON.stringify(sorted, null, 2));
              },
            },
            typescript({
              include: ["src/frontend/src/**/*.svelte"],
              check: false, // We only intend to transform, not check syntax here
              transformers: [
                () => ({
                  before: [
                    transform({
                      overrideIdFn: "[sha512:contenthash:base64:6]",
                      removeDefaultMessage: true,
                      onMsgExtracted: (_, msgs) => {
                        // Update locale file with extracted message descriptors
                        if (command === "build") {
                          const json = readFileSync(localeFile, "utf8").trim();
                          const en = json.length > 0 ? JSON.parse(json) : {};
                          msgs.forEach(
                            ({ id, defaultMessage, description }) => {
                              en[id] = { defaultMessage, description };
                            },
                          );
                          writeFileSync(
                            localeFile,
                            JSON.stringify(en, null, 2),
                          );
                        }
                      },
                    }),
                  ],
                }),
              ],
            }),
          ]
        : []),
      // Needed to support WebAuthnIdentity in this repository due to borc dependency.
      nodePolyfills({
        include: ["buffer"],
      }),
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
                      "id.ai",
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
            https: process.env.TLS_DEV_SERVER === "1" ? {} : undefined,
            proxy: {
              "/api": `http://127.0.0.1:${readReplicaPort()}`,
            },
            allowedHosts: ["icp-api.io"],
            cors: {
              origin: [
                "https://id.ai",
                "https://identity.internetcomputer.org",
                "https://identity.ic0.app",
                "https://nice-name.com",
                "https://nice-issuer-custom-orig.com",
                "https://be2us-64aaa-aaaaa-qaabq-cai.icp0.io",
                // Test app
                "https://bd3sg-teaaa-aaaaa-qaaba-cai.icp0.io",
                // Test app
                "https://bd3sg-teaaa-aaaaa-qaaba-cai.ic0.app",
                // Issuer
                "https://bkyz2-fmaaa-aaaaa-qaaaq-cai.icp0.io",
                // Issuer
                "https://bkyz2-fmaaa-aaaaa-qaaaq-cai.ic0.app",
              ],
            },
          },
  };
});
