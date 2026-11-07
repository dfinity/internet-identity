import { defineConfig } from "vite";

/**
 * The replica the dev server forwards `/api` to. Served from the canister
 * there is nothing to forward: the gateway in front of it answers.
 */
const REPLICA_URL = process.env.REPLICA_URL ?? "http://127.0.0.1:8000";

export default defineConfig({
  root: "./src",
  build: {
    outDir: "../dist",
    emptyOutDir: true,
  },
  server: {
    port: 8082,
    proxy: {
      "/api": REPLICA_URL,
    },
  },
});
