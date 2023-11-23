import { defineConfig } from "vite";
import {} from "../../vite.plugins";

export default defineConfig({
  root: ".",
  build: {
    rollupOptions: {
      input: ["index.html"],
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
    port: 5175,
  },
});
