import { execSync } from "child_process";
import { defineConfig } from "vite";

/**
 * Read a canister ID from dfx's local state
 */
export const readCanisterId = ({
  canisterName,
}: {
  canisterName: string;
}): string => {
  const command = `dfx canister id ${canisterName}`;
  try {
    const stdout = execSync(command);
    return stdout.toString().trim();
  } catch (e) {
    throw Error(
      `Could not get canister ID for '${canisterName}' with command '${command}', was the canister deployed? ${e}`
    );
  }
};

export const getReplicaHost = (): string => {
  const command = `dfx info webserver-port`;
  try {
    const stdout = execSync(command);
    const port = stdout.toString().trim();
    return `http://127.0.0.1:${port}`;
  } catch (e) {
    throw Error(
      `Could not get replica port '${command}', is the replica running? ${e}`
    );
  }
};

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
