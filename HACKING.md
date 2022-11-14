# Hacking

This document explains how to build the Wasm module of the Internet Identity canister from scratch.

> 💡 Binary releases are available on the [release page][releases].

> 💡 The simplest way to build the code is to use the [Docker build][docker-build].

> 💡 Make sure to read up on the [build features and flavors][features-and-flavors].

The build requires the following dependencies:

* [`dfx`](https://github.com/dfinity/sdk/releases/latest) version 0.8.3 or later
* [`ic-cdk-optimizer`](https://github.com/dfinity/cdk-rs/tree/main/src/ic-cdk-optimizer), which can be installed by running [./scripts/bootstrap](./scripts/bootstrap)
* Rustup with target `wasm32-unknown-unknown` (see [rustup instructions](https://rust-lang.github.io/rustup/cross-compilation.html)), which can be installed by running [./scripts/bootstrap](./scripts/bootstrap)
* Node.js v16+
* CMake

> NOTE! If you're only going to hack on the HTML and CSS code, just run `npm run showcase`. This will start a minimal web server which serves a showcase of the pages used in the app.

## Running Locally

To run the internet_identity canisters, proceed as follows after cloning the repository

```bash
npm ci
dfx start [--clean] [--background]
```

In a different terminal, run the following command to install the Internet Identity canister:

```bash
II_FETCH_ROOT_KEY=1 dfx deploy --no-wallet --argument '(null)'
```

Then the canister can be used as

```bash
$ dfx canister call internet_identity init_salt
()
$ echo $?
0
```

See `dfx canister call --help` and [the documentation](https://sdk.dfinity.org/docs/developers-guide/cli-reference/dfx-canister.html#_examples) for more information.

The `dfx` executable can proxy queries to the canister. To view it, run the following and open the resulting link in your browser:

```bash
echo "http://localhost:4943?canisterId=$(dfx canister id internet_identity)"
```

### Building the frontend

The fastest workflow to get the development environment running is to deploy once with

```bash
npm ci
dfx start [--clean] [--background]
II_FETCH_ROOT_KEY=1 dfx deploy --no-wallet --argument '(null)'
```

To serve the frontend locally via webpack (recommended during development), run
the following:

```bash
npm start
```

Then open `http://localhost:8080` in your browser. Webpack will reload the page whenever you save changes to files. To ensure your changes pass our formatting and linter checks, run the following command:

```bash
npm run format && npm run lint
```

Finally, to test workflows like authentication from a client application, you start the Selenium test app:

```bash
cd demos/selenium-test-app
npm ci
npm run start
```

Then open `http://localhost:8081` in your browser.

Make sure that the "Identity Provider" is set to "http://localhost:8080" if you
serve the Internet Identity frontend from webpack.

**NOTE on testing on LAN:**

If you are testing on LAN -- for instance, connecting to an Internet Identity
server running on your laptop from your smartphone over WiFi -- you may run
into the following issues:

* The webpage may not be accessible on LAN. By default webpack will serve the
  content using the `localhost` host. Firewall rules for `localhost` are
  somewhat strict; if you cannot access the page from devices on your LAN try
  serving with `webpack serve --host 0.0.0.0`.
* Internet Identity may tell you that your browser is not supported. The reason
  for this is that some security-focused features are only enabled on `https`
  and `localhost` pages. A workaround is to use [ngrok](http://ngrok.com) to
  forward your local port over https.

#### Test suites

We have a set of Selenium tests that run through the various flows. To set up a local deployment follow the steps in [docker-test-env/README.md](docker-test-env/README.md).
The tests can be executed by running:

```bash
npm run test:e2e
```

Or with a specific screen size e.g.:
```bash
npm run test:e2e-desktop
```

We autoformat our code using `prettier`. Running `npm run format` formats all files in the frontend.
If you open a PR that isn't formatted according to `prettier`, CI will automatically add a formatting commit to your PR.

We use `eslint` to check the frontend code. You can run it with `npm run lint`, or set up your editor to do it for you.


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

This will produce `./internet_identity.wasm`.

[releases]: https://github.com/dfinity/internet-identity/releases
[docker-build]: ./README.md#building-with-docker
[features-and-flavors]: ./README.md#build-features-and-flavors
