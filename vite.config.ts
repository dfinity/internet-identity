import { NodeModulesPolyfillPlugin } from "@esbuild-plugins/node-modules-polyfill";
import { extname, resolve } from "path";
import type { Plugin } from "rollup";
import rollupNodePolyFill from "rollup-plugin-node-polyfills";
import { defineConfig, loadEnv, UserConfig } from "vite";
import viteCompression from "vite-plugin-compression";
import {
  injectCanisterIdPlugin,
  preRenderAboutPlugin,
  stripInjectJsScript,
} from "./vite.plugins";

const defaultConfig = (mode?: string): Omit<UserConfig, "root"> => {
  const envPrefix = "II_" as const;

  // Expand environment - .env files - with canister IDs
  process.env = {
    ...process.env,
    ...loadEnv(mode ?? "development", process.cwd()),
  };

  return {
    envDir: "../../",
    envPrefix,
    resolve: {
      // TODO: use aliases for imports
      alias: {
        $assets: resolve(__dirname, "src/frontend/assets"),
        $app: resolve(__dirname, "src/frontend/src"),
        $generated: resolve(__dirname, "src/frontend/generated"),
      },
    },
    build: {
      outDir: "../../dist",
      emptyOutDir: true,
      rollupOptions: {
        plugins: [rollupNodePolyFill() as Plugin],
        input: {
          main: resolve(__dirname, "src/frontend/index.html"),
          about: resolve(__dirname, "src/frontend/about.html"),
        },
        output: {
          entryFileNames: `[name].js`,
          // II canister only supports resources that contains a single dot in their filenames. qr-creator.js.gz = ok. qr-creator.min.js.gz not ok. qr-creator.es6.min.js.gz no ok.
          chunkFileNames: ({ name }) => `${name.replace(/.es6|.min/gm, "")}.js`,
          assetFileNames: `[name].[ext]`,
        },
      },
    },
    plugins: [
      preRenderAboutPlugin(),
      [...(mode === "development" ? [injectCanisterIdPlugin()] : [])],
      [...(mode === "production" ? [stripInjectJsScript()] : [])],
      // II canister only supports one content type per resource. That is why we remove the original file.
      viteCompression({
        deleteOriginFile: true,
        filter: (file: string): boolean => ![".html"].includes(extname(file)),
      }),
    ],
    optimizeDeps: {
      esbuildOptions: {
        define: {
          global: "globalThis",
        },
        plugins: [NodeModulesPolyfillPlugin()],
      },
    },
    server: {
      port: 8080,
    },
    define: {
      // TODO: replace process.env with import
      "process.env": {
        ...process.env,
      },
    },
  };
};

// TODO: about.html
// TODO: loader.webp not copied

// https://vitejs.dev/config/
export default defineConfig(({ mode }: UserConfig): UserConfig => {
  const { build, ...rest } = defaultConfig(mode);

  if (mode === "showcase") {
    return {
      ...rest,
      root: "src/showcase",
      build: {
        ...build,
        outDir: "../../showcase",
      },
    };
  }

  return {
    ...rest,
    root: "src/frontend",
    build,
  };
});
