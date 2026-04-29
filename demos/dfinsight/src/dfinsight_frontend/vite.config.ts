import { defineConfig, loadEnv } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), "");
  const backendCanisterId =
    env.CANISTER_ID_DFINSIGHT_BACKEND ?? env.VITE_DFINSIGHT_BACKEND_CANISTER_ID;
  const host = env.VITE_IC_HOST ?? "http://127.0.0.1:4943";
  const iiUrl = env.VITE_II_URL ?? "https://id.ai/authorize?sso=dfinity.org";

  return {
    plugins: [react()],
    define: {
      "import.meta.env.VITE_DFINSIGHT_BACKEND_CANISTER_ID": JSON.stringify(
        backendCanisterId ?? "",
      ),
      "import.meta.env.VITE_IC_HOST": JSON.stringify(host),
      "import.meta.env.VITE_II_URL": JSON.stringify(iiUrl),
    },
    server: {
      port: 5173,
    },
    build: {
      target: "es2022",
      sourcemap: true,
    },
  };
});
