import { resolve } from "path";
import { defineConfig, UserConfig } from "vite";
import {
  compression,
  injectCanisterIdPlugin,
  stripInjectJsScript,
} from "./vite.plugins";

const defaultConfig = (mode?: string): Omit<UserConfig, "root"> => {
  // Path "../../" have to be expressed relative to the "root".
  // e.g.
  // root = src/frontend
  // outDir = ../../dist
  return {
    publicDir: "assets",
    envPrefix: "II_",
    resolve: {
      alias: {
        // Polyfill stream for the browser. e.g. needed in "Recovery Phrase" features.
        stream: "stream-browserify",
        // Custom alias we are using to shorten and make absolute the imports
        $generated: resolve(__dirname, "src/frontend/generated"),
        $src: resolve(__dirname, "src/frontend/src"),
      },
    },
    build: {
      outDir: "../../dist",
      emptyOutDir: true,
      rollupOptions: {
        // Bundle only english words in bip39.
        external: /.*\/wordlists\/(?!english).*\.json/,
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
      [
        ...(mode === "production"
          ? [stripInjectJsScript(), compression()]
          : []),
      ],
    ],
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
  };
};

// https://vitejs.dev/config/
export default defineConfig(({ mode }: UserConfig): UserConfig => {
  const { build, ...rest } = defaultConfig(mode);

  if (mode === "showcase") {
    return {
      ...rest,
      root: "src/showcase",
      publicDir: "../frontend/assets",
      build: {
        ...build,
        outDir: "../../dist-showcase",
      },
    };
  }

  return {
    ...rest,
    root: "src/frontend",
    build,
  };
});
