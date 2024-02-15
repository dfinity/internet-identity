# Hacking

This document explains how to build the Wasm module of the Internet Identity canister from scratch.

> 💡 Binary releases are available on the [release page][releases].

> 💡 The simplest way to build the code is to use the [Docker build][docker-build].

> 💡 Make sure to read up on the [build features and flavors][features-and-flavors].

The build requires the following dependencies:

* [`dfx`](https://github.com/dfinity/sdk/releases/latest) version 0.10.0 or later
* Rustup with target `wasm32-unknown-unknown` (see [rustup instructions](https://rust-lang.github.io/rustup/cross-compilation.html)), which can be installed by running [./scripts/bootstrap](./scripts/bootstrap)
* CMake
* [`ic-wasm`](https://github.com/dfinity/ic-wasm), which can be installed by running [./scripts/bootstrap](./scripts/bootstrap)
* Node.js v16+

> NOTE! If you're only going to hack on the HTML and CSS code, see the [showcase](#showcase) section.

## Running Locally

To run the Internet Identity canister, proceed as follows after cloning the repository

```bash
npm ci
dfx start [--clean] [--background]
```

In a different terminal, run the following command to install the Internet Identity canister:

```bash
dfx deploy internet_identity --no-wallet
```

> [!NOTE]\
> By default, a dummy (fixed) CAPTCHA is used. If you want to use the real (random) CAPTCHA, set
> `II_DUMMY_CAPTCHA` to `0`:\
> `II_DUMMY_CAPTCHA=0 dfx deploy internet_identity --no-wallet`


Then the canister can be used as

```bash
$ dfx canister call internet_identity stats
(
  record { ... }
)
```

See `dfx canister call --help` and [the documentation](https://sdk.dfinity.org/docs/developers-guide/cli-reference/dfx-canister.html#_examples) for more information.

The `dfx` executable can proxy queries to the canister. To view it, run the following and open the resulting link in your browser:

```bash
echo "http://$(dfx canister id internet_identity).localhost:4943"
```

*Note: The URL doesn't work for safari.*

### Building the frontend

The fastest workflow to get the development environment running is to deploy once with

```bash
npm ci
dfx start [--clean] [--background]
dfx deploy internet_identity --no-wallet
```

To serve the frontend locally (recommended during development), run
the following:

```bash
npm run dev
```

Then open `http://localhost:5173` in your browser. The page is reloaded whenever you save changes to files.

**NOTE on testing on LAN:**

If you are testing on LAN -- for instance, connecting to an Internet Identity
server running on your laptop from your smartphone over WiFi -- you may run
into the following issues:

- The webpage may not be accessible on LAN. By default the development server will serve the
  content using the `localhost` host. Firewall rules for `localhost` are
  somewhat strict; if you cannot access the page from devices on your LAN try
  serving with `npm run host`.
- Internet Identity may tell you that your browser is not supported. The reason
  for this is that some security-focused features are only enabled on `https`
  and `localhost` pages. A workaround is to use [ngrok](http://ngrok.com) to
  forward your local port over https.

#### Test suites

We have a set of Selenium tests that run through the various flows. To set up a local deployment follow these steps:
1. Start a local replica with `dfx start`
1. Deploy II and the other test canisters with `dfx deploy --no-wallet`
1. Start the vite dev server with TLS enabled: `TLS_DEV_SERVER=1 npm run dev`

To watch the tests run in the browser remove the `headless` option from `src/frontend/src/test-e2e/util.ts`.

The tests can be executed by running:

```bash
npm run test:e2e
```

We autoformat our code using `prettier`. Running `npm run format` formats all files in the frontend.
If you open a PR that isn't formatted according to `prettier`, CI will automatically add a formatting commit to your PR.

We use `eslint` to check the frontend code. You can run it with `npm run lint`.


### Building the backend

The Internet Identity backend is a Wasm canister implemented in Rust and built from the `internet_identity` cargo package (`src/internet_identity`).

Run the following command in the root of the repository to execute the test suites of all the libraries:

```bash
cargo test
```

The backend canister is also used to serve the frontend assets.
This creates a dependency between the frontend and the backend.
So running the usual `cargo build --target wasm32-unknown-unknown -p internet_identity` might not work or include an outdated version of the frontend.

Use the following command to build the backend canister Wasm file instead:

```bash
dfx build internet_identity
```

This will produce `./internet_identity.wasm.gz`.

## Attribute Sharing / Verifiable Credentials

To experiment with Attribute Sharing / Verifiable Credentials feature, one can start a demo VC-issuer by running
```bash
dfx deploy issuer
```
This will deploy also `internet_identity`, and provision the issuer for the testing environment.
See [VC issuer documentation](./demos/vc_issuer/README.md) for details.

Our [`test-app`](./demos/test-app) offers a simple relying party functionality and can be deployed using
```bash
dfx deploy test_app
```

Afterward one can serve the frontends locally via:
```bash
npm run dev
```
and access the issuer FE at http://issuer.localhost:5173/, and the test-app at http://test_app.localhost:5173/
(the relying party is functionality is at the bottom of the page).

## Showcase

The simplest way to make visual changes (HTML & CSS, and non-flow JS) is to start the showcase:

``` bash
npm run showcase
```

This will start a webserver showcasing most II pages & components. The showcase can also be built:

``` bash
npm run build:showcase [--base 'some-base/']
npm run preview:showcase [--base 'some-base/']
```


[releases]: https://github.com/dfinity/internet-identity/releases
[docker-build]: ./README.md#building-with-docker
[features-and-flavors]: ./README.md#build-features-and-flavors
