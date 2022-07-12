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
  },
  resolve: {
    alias: {
      process: "process/browser",
    },
    fallback: {
      assert: require.resolve("assert/"),
      buffer: require.resolve("buffer/"),
      events: require.resolve("events/"),
      stream: require.resolve("stream-browserify/"),
      util: require.resolve("util/"),
    },
  },
  devtool: "source-map",
  devServer: {
    // Set up a proxy that redirects API calls and /index.html to the
    // replica; the rest we serve from here.
    setupMiddlewares: (middlewares, devServer) => {
      const dfxJson = "./dfx.json";
      let replicaHost;

      try {
        replicaHost = require(dfxJson).networks.local.bind;
      } catch (e) {
        throw Error(`Could get host from ${dfxJson}: ${e}`);
      }
      // If the replicaHost lacks protocol (e.g. 'localhost:8000') the
      // requests are not forwarded properly
      if (!replicaHost.startsWith("http://")) {
        replicaHost = `http://${replicaHost}`;
      }
      const canisterIdsJson = "./.dfx/local/canister_ids.json";
      let canisterId;
      try {
        canisterId = require(canisterIdsJson).selenium_test_app.local;
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
    port: 8080,
    proxy: {
      // Make sure /api calls land on the replica (and not on webpack)
      "/api": "http://localhost:8000",
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
