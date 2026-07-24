import { defineConfig } from "vite";

// Minimal dev/build config. `--host` lets a Cloudflare tunnel (or any
// reverse proxy) reach the dev server; `allowedHosts: true` accepts the
// tunnel's Host header.
export default defineConfig({
  server: {
    host: true,
    allowedHosts: true,
  },
});
