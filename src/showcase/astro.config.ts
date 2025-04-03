import { defineConfig } from "astro/config";
import path from "path";
import { fileURLToPath } from "url";
import { nodePolyfills } from "vite-plugin-node-polyfills";

const ROOT = "../..";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// https://astro.build/config
export default defineConfig({
  publicDir: path.join(ROOT, "src/frontend/assets"),
  outDir: path.join(ROOT, "dist-showcase"),
  server: {
    port: 5174,
  },
  vite: {
    resolve: {
      alias: {
        $lib: path.resolve(__dirname, "../frontend/src/lib"),
      },
    },
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
