import { defineConfig } from "vite";

// A minimal Vite dev server for the push-notifications smoke test.
// Serves `src/index.html` at the root and copies anything under
// `public/` (currently just `service-worker.js`) to the origin root so
// the browser can register the SW at `/service-worker.js`.
export default defineConfig({
  root: "src",
  publicDir: "../public",
  server: {
    port: 5175,
    host: "localhost",
  },
  build: {
    outDir: "../dist",
    emptyOutDir: true,
  },
});
