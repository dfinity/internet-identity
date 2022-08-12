const path = require("path");
const webpack = require("webpack");
const CopyPlugin = require("copy-webpack-plugin");
const TerserPlugin = require("terser-webpack-plugin");
const CompressionPlugin = require("compression-webpack-plugin");
const HttpProxyMiddlware = require("http-proxy-middleware");
const dfxJson = require("./dfx.json");
require("dotenv").config();

/** Read the replica host from dfx.json */
function readReplicaHost() {
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

  return replicaHost;
}

/** Read the II canister ID from dfx's local state */
function readCanisterId() {
  const canisterIdsJson = "./.dfx/local/canister_ids.json";

  let canisterId;

  try {
    canisterId = require(canisterIdsJson).internet_identity.local;
  } catch (e) {
    throw Error(`Could get canister ID from ${canisterIdsJson}: ${e}`);
  }

  console.log("Read canister ID:", canisterId);

  return canisterId;
}

const isProduction = process.env.NODE_ENV === "production";
const devtool = isProduction ? undefined : "source-map";

/**
 * Generate a webpack configuration for a canister.
 */
module.exports = {
  mode: isProduction ? "production" : "development",
  entry: {
    index: path.join(__dirname, "src", "frontend", "src", "index"),
  },
  devtool,
  optimization: {
    minimize: isProduction,
  },
  resolve: {
    extensions: [".js", ".ts", ".jsx", ".tsx"],
    fallback: {
      assert: require.resolve("assert/"),
      buffer: require.resolve("buffer/"),
      events: require.resolve("events/"),
      stream: require.resolve("stream-browserify/"),
      util: require.resolve("util/"),
    },
  },
  output: {
    filename: "[name].js",
    path: path.join(__dirname, "dist"),
  },
  devServer: {
    // Set up a proxy that redirects API calls and /index.html to the
    // replica; the rest we serve from here.
    onBeforeSetupMiddleware: (devServer) => {
      // canisterId singleton, read lazily
      let canisterId = null;

      // basically everything _except_ for index.js, because we want live reload
      devServer.app.get(
        ["/", "/index.html", "/faq", "/about"],
        HttpProxyMiddlware.createProxyMiddleware({
          // Read the replica host from dfx.json. The replica does not need to be running, which is convenient
          // in case we just load pages that don't talk to the canister (then we wouldn't want to have to start the replica or build the canister)
          target: readReplicaHost(),

          // For path rewriting we use a function that lazily (i.e. when first called, using a singleton) reads
          // the canister ID from the local dfx state and returns a new request path that includes
          // the ?canisterId=foo atrocity
          pathRewrite: (pathAndParams, req) => {
            let queryParamsString = `?`;

            const [path, params] = pathAndParams.split("?");

            if (params) {
              queryParamsString += `${params}&`;
            }

            if (canisterId === null) {
              canisterId = readCanisterId();
            }

            queryParamsString += `canisterId=${canisterId}`;

            return path + queryParamsString;
          },
        })
      );
    },
    port: 8080,
    proxy: {
      // Make sure /api calls land on the replica (and not on webpack)
      "/api": "http://localhost:8000",
    },
    allowedHosts: [".localhost", ".local", ".ngrok.io"],
  },

  module: {
    rules: [
      { test: /\.(ts|tsx)$/, loader: "ts-loader" },
      { test: /\.css$/, use: ["style-loader", "css-loader"] },
      {
        test: /\.(png|jpg|gif)$/i,
        type: "asset/resource",
      },
    ],
  },
  plugins: [
    new CopyPlugin({
      patterns: [
        {
          from: path.join(__dirname, "src", "frontend", "assets"),
          to: path.join(__dirname, "dist"),
        },
      ],
    }),
    new webpack.ProvidePlugin({
      Buffer: [require.resolve("buffer/"), "Buffer"],
      process: require.resolve("process/browser"),
    }),
    new webpack.EnvironmentPlugin({
      II_FETCH_ROOT_KEY: "0",
      II_DUMMY_AUTH: "0",
      II_DUMMY_CAPTCHA: "0",
    }),
    new CompressionPlugin({
      test: /\.js(\?.*)?$/i,
    }),
    new webpack.IgnorePlugin(/^\.\/wordlists\/(?!english)/, /bip39\/src$/),
  ],
};
