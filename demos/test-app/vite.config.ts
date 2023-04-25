import { readFileSync } from "fs";
import { join } from "path";
import { defineConfig, type UserConfig } from "vite";

const replicaHost = "http://127.0.0.1:4943" as const;

const rewriteRoute = (pathAndParams: string): string => {
  const readCanisterId = (): string => {
    const canisterIdsJson = join(
        process.cwd(),
        ".dfx",
        "local",
        "canister_ids.json"
    );
    try {
      const buffer = readFileSync(canisterIdsJson);
      const {
        test_app: { local },
      } = JSON.parse(buffer.toString("utf-8"));
      return local;
    } catch (e: unknown) {
      throw Error(`Could get canister ID from ${canisterIdsJson}: ${e}`);
    }
  };

  let queryParamsString = `?`;

  const [path, params] = pathAndParams.split("?");

  if (params) {
    queryParamsString += `${params}&`;
  }

  queryParamsString += `canisterId=${readCanisterId()}`;

  return path + queryParamsString;
};

export default defineConfig(
  ({ mode }: UserConfig): UserConfig => ({
    root: "src",
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
    server: {
      port: 8081,
      // Set up a proxy that redirects API calls and /index.html to the
      // replica; the rest we serve from here.
      proxy: {
        "/api": replicaHost,
        "/": {
          target: replicaHost,
          rewrite: rewriteRoute,
        },
        "/index.html": {
          target: replicaHost,
          rewrite: rewriteRoute,
        },
        "/.well-known/ii-alternative-origins": {
          target: replicaHost,
          rewrite: rewriteRoute,
        },
        "/.well-known/evil-alternative-origins": {
          target: replicaHost,
          rewrite: rewriteRoute,
        },
      },
    },
  })
);
