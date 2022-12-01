import path from "path";
import * as fs from "fs";
import webpack from "webpack";
import CopyPlugin from "copy-webpack-plugin";
import CompressionPlugin from "compression-webpack-plugin";
import HtmlWebpackPlugin from "html-webpack-plugin";
import MiniCssExtractPlugin from "mini-css-extract-plugin";
import CssMinimizerPlugin from "css-minimizer-webpack-plugin";
import { fileURLToPath } from "url";
import { config } from "dotenv";

const __dirname = fileURLToPath(new URL(".", import.meta.url));

config();

/** Read the II canister ID from dfx's local state */
function readCanisterId() {
  const canisterIdsJson = "./.dfx/local/canister_ids.json";

  let canisterId;

  try {
    canisterId = JSON.parse(fs.readFileSync(canisterIdsJson).toString())
      .internet_identity.local;
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
  apply(compiler: webpack.Compiler) {
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

/* Default configuration */
const defaults = {
  mode: isProduction ? "production" : "development",
  devtool,
  optimization: {
    minimize: isProduction,
    minimizer: [new CssMinimizerPlugin(), "..."],
  },
  output: { clean: true },
  resolve: {
    extensions: [".js", ".ts"],
    fallback: {
      stream: "stream-browserify",
    },
  },
  module: {
    rules: [
      { test: /\.(ts)$/, loader: "ts-loader" },
      {
        test: /\.css$/,
        use: [
          isProduction ? MiniCssExtractPlugin.loader : "style-loader",
          "css-loader",
        ],
      },
      {
        test: /\.(png|jpg|gif)$/i,
        type: "asset/resource",
      },
    ],
  },
  plugins: [
    ...(isProduction ? [new MiniCssExtractPlugin()] : []),
    new webpack.ProvidePlugin({
      Buffer: ["buffer", "Buffer"],
      process: "process/browser",
    }),
    new webpack.EnvironmentPlugin({
      II_FETCH_ROOT_KEY: "0",
      II_DUMMY_AUTH: "0",
      II_DUMMY_CAPTCHA: "0",
    }),
    new CompressionPlugin({
      test: /\.js(\?.*)?$/i,
    }),
    new webpack.IgnorePlugin({
      checkResource(resource) {
        return /.*\/wordlists\/(?!english).*\.json/.test(resource);
      },
    }),
    new CopyPlugin({
      patterns: [
        {
          from: path.join(__dirname, "src", "frontend", "assets"),
          to: path.join(__dirname, "dist"),
          // We want the html file from HtmlWebpackPlugin, not the original one
          filter: (resourcePath) => {
            return !resourcePath.endsWith("index.html");
          },
        },
      ],
    }),

    new HtmlWebpackPlugin({
      template: "src/frontend/assets/index.html",
      // Don't inject the index.js in production, because the canister actually injects it (see http.rs for more details)
      // When true, injects (a hot-reloading version of) the built javascript, which in turn inserts the CSS (through "style-loader").
      // This value is read by the template; see index.html for more details.
      inject: !isProduction,
    }),
  ],
};

export default [
  {
    ...defaults,
    name: "app",
    entry: {
      index: path.join(__dirname, "src", "frontend", "src", "index"),
    },
    devServer: {
      magicHtml: false,
      static: false,
      // Set up a proxy that redirects API calls to the replica.
      proxy: {
        // Make sure /api calls land on the replica (and not on webpack)
        "/api": "http://localhost:4943",
      },
    },
    plugins: [
      ...defaults.plugins,
      // Inject canister ID when using the dev server, so that the local file can be used
      // (instead of the HTML served by the canister)
      ...(isProduction ? [] : [new InjectCanisterIdPlugin()]),
    ],
  },
  {
    ...defaults,
    name: "showcase",
    entry: {
      showcase: path.join(__dirname, "src", "frontend", "src", "showcase"),
    },
    devServer: {
      magicHtml: false,
      static: false,
      historyApiFallback: true, // serves the app on all routes, which we use because the app itself does the routing
    },
  },
];
