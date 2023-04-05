import CompressionPlugin from "compression-webpack-plugin";
import CopyPlugin from "copy-webpack-plugin";
import CssMinimizerPlugin from "css-minimizer-webpack-plugin";
import * as fs from "fs";
import HtmlWebpackPlugin from "html-webpack-plugin";
import MiniCssExtractPlugin from "mini-css-extract-plugin";
import path from "path";
import { fileURLToPath } from "url";
import webpack from "webpack";

import { render } from "@lit-labs/ssr/lib/render-lit-html";
import { TemplateResult } from "lit-html";
import { pageContent as aboutStaticContent } from "./src/frontend/src/flows/about";
import { I18n } from "./src/frontend/src/i18n";

const __dirname = fileURLToPath(new URL(".", import.meta.url));

const i18n = new I18n();

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

// A plugin that replaces content in HTML files
class HtmlReplacePlugin {
  constructor(
    /** The function to apply on the HTML */
    private f: (html: string) => string,
    /** If specified, only replace HTML in this file */
    private name?: string
  ) {}

  apply(compiler: webpack.Compiler) {
    compiler.hooks.compilation.tap(this.constructor.name, (compilation) => {
      HtmlWebpackPlugin.getHooks(compilation).beforeEmit.tapAsync(
        this.constructor.name,
        (data, cb) => {
          if (this.name === undefined || this.name === data.outputName) {
            data.html = this.f(data.html);
          }
          cb(null, data);
        }
      );
    });
  }
}

// A plugin that creates an HTML page based on our index.html template
const htmlPlugin = ({ filename }: { filename: string }): HtmlWebpackPlugin =>
  new HtmlWebpackPlugin({
    template: "src/frontend/assets/index.html",
    // Don't inject the index.js in production, because the canister actually injects it (see http.rs for more details)
    // When true, injects (a hot-reloading version of) the built javascript, which in turn inserts the CSS (through "style-loader").
    // This value is read by the template; see index.html for more details.
    inject: !isProduction,
    filename,
  });

// A plugin that creates a static page by injecting a static TemplateResult into a page
const staticPagePlugin = (
  pageName: string,
  pageContent: TemplateResult
): HtmlReplacePlugin =>
  new HtmlReplacePlugin((html) => {
    const content = Array.from(render(pageContent)).reduce(
      (acc, v) => acc + v,
      ""
    );
    return html.replace(
      '<main id="pageContent" class="l-wrap" aria-live="polite"></main>',
      `<main id="pageContent" class="l-wrap" aria-live="polite">${content}</main>`
    );
  }, pageName);

// This emulates the behaviour of http.rs while using webpack dev server locally
// so we don't need to proxy to the backend canister for the index.html file.
// This overcomes some issues with Safari and the CSP headers set in http.rs.
const injectCanisterIdPlugin = () =>
  new HtmlReplacePlugin((html) =>
    html.replace(
      '<script id="setupJs"></script>',
      `<script data-canister-id="${readCanisterId()}" id="setupJs"></script>`
    )
  );

const staticAboutPlugin = () =>
  staticPagePlugin("about.html", aboutStaticContent(i18n));

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
      // Whether or not static pages should be hydrated
      // (only done in production since we don't have statically rendered
      // pages for development)
      HYDRATE_STATIC_PAGES: isProduction ? "1" : "0",
      // Feature flags (see README)
      II_FETCH_ROOT_KEY: "0",
      II_DUMMY_AUTH: "0",
      II_DUMMY_CAPTCHA: "0",

      II_VERSION: "",
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
          // We want html files from HtmlWebpackPlugin, not the original ones
          filter: (resourcePath) => {
            return !resourcePath.endsWith(".html");
          },
        },
      ],
    }),

    htmlPlugin({ filename: "index.html" }),
    htmlPlugin({ filename: "about.html" }),
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
      historyApiFallback: {
        // Make sure that visiting links like `/about` serves the correct HTML
        // (could also be `/index.html` since the content is the same in development, but it's
        // less confusing to show the correct file)
        rewrites: [{ from: /\/about/, to: "/about.html" }],
      },
    },
    plugins: [
      ...defaults.plugins,
      // In production, we generate static versions of our static pages (during development we
      // prefer hot-reloading dynamic pages);
      // in development, we inject canister ID when using the dev server, so that the local
      // file can be used (instead of the HTML served by the canister)
      ...(isProduction ? [staticAboutPlugin()] : [injectCanisterIdPlugin()]),
    ],
  },
  {
    ...defaults,
    name: "showcase",
    plugins: [
      ...defaults.plugins,
      new webpack.EnvironmentPlugin({
        BASE_URL: "",
      }),
    ],
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
