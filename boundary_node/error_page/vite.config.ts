import { defineConfig } from "vite";

export default defineConfig({
  root: __dirname,
  server: {
    port: 5174, // Using a different port than the main app
  },
});
