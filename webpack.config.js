const path = require("path");
const webpack = require("webpack");
const CopyPlugin = require("copy-webpack-plugin");
const TerserPlugin = require("terser-webpack-plugin");
const CompressionPlugin = require("compression-webpack-plugin");
const HttpProxyMiddlware = require("http-proxy-middleware");
const dfxJson = require("./dfx.json");
require("dotenv").config();

/**
 * Generate a webpack configuration for a canister.
 */
function generateWebpackConfigForCanister(name, info) {
  const isProduction = process.env.NODE_ENV === "production";
  const devtool = isProduction ? undefined : "source-map";

  return {
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
          const dfxJson = './dfx.json';
          let replicaHost;

          try {
              replicaHost = require(dfxJson).networks.local.bind;
          } catch (e) {
              throw Error(`Could get host from ${dfxJson}: ${e}`);
          }

          // If the replicaHost lacks protocol (e.g. 'localhost:8000') the
          // requests are not forwarded properly
          if(!replicaHost.startsWith("http://")) {
              replicaHost = `http://${replicaHost}`;
          }

          const canisterIdsJson = './.dfx/local/canister_ids.json';

          let canisterId;

          try {
              canisterId = require(canisterIdsJson).internet_identity.local;
          } catch (e) {
              throw Error(`Could get canister ID from ${canisterIdsJson}: ${e}`);
          }

          // basically everything _except_ for index.js, because we want live reload
          devServer.app.get(['/', '/index.html', '/faq', '/faq', 'about' ], HttpProxyMiddlware.createProxyMiddleware( {
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

          }));
      },
      port: 8080,
      proxy: {
        // Make sure /api calls land on the replica (and not on webpack)
        "/api": "http://localhost:8000",
      },
      allowedHosts: [".localhost", ".local", ".ngrok.io"],
    },

    // Depending in the language or framework you are using for
    // front-end development, add module loaders to the default
    // webpack configuration. For example, if you are using React
    // modules and CSS as described in the "Adding a stylesheet"
    // tutorial, uncomment the following lines:
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
        "II_FETCH_ROOT_KEY": "0",
        "II_DUMMY_AUTH": "0",
        "II_DUMMY_CAPTCHA": "0",
      }),
      new CompressionPlugin({
        test: /\.js(\?.*)?$/i,
      }),
      new webpack.IgnorePlugin(/^\.\/wordlists\/(?!english)/, /bip39\/src$/),
    ],
  };
}

// If you have additional webpack configurations you want to build
//  as part of this configuration, add them to the section below.
module.exports = [
  ...Object.entries(dfxJson.canisters)
    .map(([name, info]) => {
      return generateWebpackConfigForCanister(name, info);
    })
    .filter((x) => !!x),
];
