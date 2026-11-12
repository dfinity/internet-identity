import { defineConfig, type PluginOption, type UserConfig } from "vite";
import baseConfig from "./vite.config";

// Serves the UI concept at `/concept` on mock data, with no local replica.
// The base config forwards requests to the replica and proxies `/api` to it,
// both of which ask the `icp` CLI for a running network at server start.
// Run with `II_CONCEPT=1` (see `npm run dev:concept`), which the client and
// server hooks read to skip the canister config they would otherwise load.
export default defineConfig((env): UserConfig => {
  const base = (baseConfig as (env: unknown) => UserConfig)(env);
  const plugins = (base.plugins ?? []).filter(
    (plugin: PluginOption) =>
      !(
        plugin !== null &&
        typeof plugin === "object" &&
        "name" in plugin &&
        plugin.name === "replica-forward"
      ),
  );
  return {
    ...base,
    plugins,
    server: {
      // `localhost` can resolve to `::1` alone, which leaves a browser or
      // proxy dialing 127.0.0.1 with nothing to connect to.
      host: "127.0.0.1",
      port: 5174,
      strictPort: true,
      allowedHosts: true,
      // File events don't always reach the watcher (e.g. in a sandboxed shell).
      watch: { usePolling: true, interval: 300 },
    },
  };
});
