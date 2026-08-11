// Bundle the Web Push service worker (src/frontend/sw-src/service-worker.ts)
// into src/frontend/static/service-worker.js, so it can pull in agent-js and
// authenticate to a dApp to fetch notification content. Run by `npm run
// build:sw`, and as a prebuild step of `npm run build`. For local dev, run it
// once (or on change) before serving — static/ is served verbatim by vite.
import esbuild from "esbuild";

const watch = process.argv.includes("--watch");

const options = {
  entryPoints: ["src/frontend/sw-src/service-worker.ts"],
  outfile: "src/frontend/static/service-worker.js",
  bundle: true,
  format: "iife",
  platform: "browser",
  target: "es2020",
  minify: !watch,
  sourcemap: watch,
  // agent-js is written for a browser: it reaches for `global` and `window`
  // (window.crypto, window.location), neither of which exists in a worker.
  // Map `global` to globalThis, and bind a local `window = globalThis` at the
  // top of the IIFE so those property reads resolve to the worker's own
  // crypto/location. A service worker's globalThis carries both.
  define: { global: "globalThis" },
  banner: { js: "var window = globalThis;" },
  logLevel: "info",
};

if (watch) {
  const ctx = await esbuild.context(options);
  await ctx.watch();
  console.log("[build-sw] watching service worker source…");
} else {
  await esbuild.build(options);
  console.log("[build-sw] wrote src/frontend/static/service-worker.js");
}
