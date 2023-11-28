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

/**
 * Inject the canister ID of 'canisterName' as a <script /> tag in index.html for local development. Will process
 * at most 1 script tag.
 */
export const injectCanisterIdPlugin = ({
  canisterName,
}: {
  canisterName: string;
}): {
  name: "html-transform";
  transformIndexHtml(html: string): string;
} => ({
  name: "html-transform",
  transformIndexHtml(html): string {
    const rgx = /<script type="module" src="(?<src>[^"]+)"><\/script>/;
    const canisterId = readCanisterId({ canisterName });

    return html.replace(rgx, (_match, src) => {
      return `<script data-canister-id="${canisterId}" type="module" src="${src}"></script>`;
    });
  },
});

export default defineConfig(({ command, mode }) => ({
  root: "./app",
  build: {
    outDir: "../dist" /* relative to 'root' */,
    emptyOutDir: true /* needed because 'outDir' is outside of 'root' */,
    rollupOptions: {
      input: ["./app/index.html"],
      /* The issuer canister needs stable names */
      output: {
        entryFileNames: `[name].js`,
        chunkFileNames: `[name].js`,
        assetFileNames: `[name].[ext]`,
      },
    },
  },
  plugins: [
    ...(mode === "development"
      ? [injectCanisterIdPlugin({ canisterName: "issuer" })]
      : []),
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
      ? undefined
      : {
          port: 5175,
          proxy: {
            "/api": getReplicaHost(),
          },
        },
}));
