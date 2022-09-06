const path = require("path");
const webpack = require("webpack");
const CopyPlugin = require("copy-webpack-plugin");
const CompressionPlugin = require("compression-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
require("dotenv").config();

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

// This plugin emulates the behaviour of http.rs while using webpack dev server locally
// so we don't need to proxy to the backend canister for the index.html file.
// This overcomes some issues with Safari and the CSP headers set in http.rs.
class InjectCanisterIdPlugin {
  apply(compiler) {
    compiler.hooks.compilation.tap("InjectCanisterIdPlugin", (compilation) => {
      HtmlWebpackPlugin.getHooks(compilation).beforeEmit.tapAsync(
        "InjectCanisterIdPlugin",
        (data, cb) => {
          data.html = data.html.replace(
            '<script id="setupJs"></script>',
            `<script data-canister-id="${readCanisterId()}" id="setupJs"></script>`
          );

          cb(null, data);
        }
      );
    });
  }
}

const isProduction = process.env.NODE_ENV === "production";
const devtool = isProduction ? undefined : "source-map";

/**
 * Generate a webpack configuration for a canister.
 */

module.exports = [
  {
    mode: isProduction ? "production" : "development",
    name: "app", // TODO: forward this where necessary
    entry: {
      index: path.join(__dirname, "src", "frontend", "src", "index"),
    },
    devtool,
    optimization: {
      minimize: isProduction,
    },
    resolve: {
      extensions: [".js", ".ts"],
      fallback: {
        stream: require.resolve("stream-browserify/"),
      },
    },
    output: {
      filename: "[name].js",
      path: path.join(__dirname, "dist"),
    },
    devServer: {
      // Set up a proxy that redirects API calls to the replica.
      port: 8080,
      proxy: {
        // Make sure /api calls land on the replica (and not on webpack)
        "/api": "http://localhost:8000",
      },
      allowedHosts: [".localhost", ".local", ".ngrok.io"],
    },

    module: {
      rules: [
        { test: /\.(ts)$/, loader: "ts-loader" },
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
            // allow HtmlWebpackPlugin to handle serving the index.html file locally
            // this avoids some Safari issues with the CSP headers served by http.rs
            filter: isProduction
              ? undefined
              : async (resourcePath) => {
                  return !resourcePath.endsWith("index.html");
                },
          },
        ],
      }),
      // allow HtmlWebpackPlugin to handle serving the index.html file locally
      // this avoids some Safari issues with the CSP headers served by http.rs
      ...(isProduction
        ? []
        : [
            new HtmlWebpackPlugin({
              template: "src/frontend/assets/index.html",
            }),
            new InjectCanisterIdPlugin(),
          ]),
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
  },
  {
    mode: isProduction ? "production" : "development",
    name: "showcase",
    entry: {
      showcase: path.join(__dirname, "src", "frontend", "src", "showcase"),
    },
    devtool,
    resolve: {
      extensions: [".js", ".ts"],
      fallback: {
        stream: require.resolve("stream-browserify/"),
      },
    },
    output: {
      filename: "[name].js",
      path: path.join(__dirname, "dist"),
    },
    devServer: {
      port: 8080,
      historyApiFallback: true, // serves the app on all routes, which we use because the app itself does the routing
    },
    module: {
      rules: [
        { test: /\.(ts)$/, loader: "ts-loader" },
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
            filter: async (resourcePath) => {
              return !resourcePath.endsWith("index.html");
            },
          },
        ],
      }),
      new HtmlWebpackPlugin(),
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
  },
];
