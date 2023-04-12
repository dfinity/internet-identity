import {defineConfig, loadEnv, UserConfig} from "vite";
import { NodeModulesPolyfillPlugin } from "@esbuild-plugins/node-modules-polyfill";
import rollupNodePolyFill from "rollup-plugin-node-polyfills";
import { injectCanisterIdPlugin, stripInjectJsScript } from "./vite.plugins";
import { resolve } from "path";
import type { Plugin } from "rollup";
import viteCompression from "vite-plugin-compression";

const defaultConfig = (mode?: string): Omit<UserConfig, "root"> => {
  const envPrefix = "II_" as const;

  // Expand environment - .env files - with canister IDs
  process.env = {
    ...process.env,
    ...loadEnv(mode ?? 'development', process.cwd()),
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
        output: {
          entryFileNames: `[name].js`,
          chunkFileNames: `[name].js`,
          assetFileNames: `[name].[ext]`,
        },
      },
    },
    plugins: [
      viteCompression(),
      [...(mode === "development" ? [injectCanisterIdPlugin()] : [])],
      [...(mode === "production" ? [stripInjectJsScript()] : [])],
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
      'process.env': {
        ...process.env
      }
    }
  }
};

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
