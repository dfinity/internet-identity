const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const webpack = require("webpack");
const HttpProxyMiddlware = require("http-proxy-middleware");

module.exports = {
  entry: {
    bundle: path.join(__dirname, "src/main.js"),
  },
  mode: "development",
  target: "web",
  output: {
    path: path.join(__dirname, "dist"),
    chunkLoadingGlobal: "wpJsonpTestApp",
  },
  resolve: {
    alias: {
      process: "process/browser",
    },
    fallback: {
      buffer: require.resolve("buffer/"),
      stream: require.resolve("stream-browserify/"),
    },
  },
  devtool: "source-map",
  devServer: {
    // Set up a proxy that redirects API calls and /index.html to the
    // replica; the rest we serve from here.
    setupMiddlewares: (middlewares, devServer) => {
      const replicaHost = "http://127.0.0.1:4943";

      const canisterIdsJson = "./.dfx/local/canister_ids.json";
      let canisterId;
      try {
        canisterId = require(canisterIdsJson).test_app.local;
      } catch (e) {
        throw Error(`Could get canister ID from ${canisterIdsJson}: ${e}`);
      }

      devServer.app.get(
        [
          "/",
          "/index.html",
          "/.well-known/ii-alternative-origins",
          "/.well-known/evil-alternative-origins",
        ],
        HttpProxyMiddlware.createProxyMiddleware({
          target: replicaHost,
          pathRewrite: (pathAndParams, req) => {
            let queryParamsString = `?`;

            const [path, params] = pathAndParams.split("?");

            if (params) {
              queryParamsString += `${params}&`;
            }

            queryParamsString += `canisterId=${canisterId}`;

            return path + queryParamsString;
          },
        })
      );
      return middlewares;
    },
    port: 8081,
    proxy: {
      // Make sure /api calls land on the replica (and not on webpack)
      "/api": "http://127.0.0.1:4943",
    },
    allowedHosts: [".localhost", ".local", ".ngrok.io"],
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: "src/index.html",
      filename: "index.html",
    }),
    new webpack.ProvidePlugin({
      Buffer: [require.resolve("buffer/"), "Buffer"],
      process: require.resolve("process/browser"),
    }),
  ],
};
