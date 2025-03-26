import { defineConfig } from "astro/config";
import path from "path";
import { nodePolyfills } from "vite-plugin-node-polyfills";

const ROOT = "../..";

// https://astro.build/config
export default defineConfig({
  publicDir: path.join(ROOT, "src/frontend/assets"),
  outDir: path.join(ROOT, "dist-showcase"),
  server: {
    port: 5174,
  },
  vite: {
    optimizeDeps: {
      esbuildOptions: {
        define: {
          global: "globalThis",
        },
      },
    },
    plugins: [
      nodePolyfills({
        include: ["buffer"],
      }),
    ],
  },
});
