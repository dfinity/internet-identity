# Internet Identity Service

See `./docs/internet-identity-spec.adoc` for a details specification and technical
documentation.

## Official build

**NOTE:** You can customize the build by using [build flavors](#build-flavors).

The official build should ideally be reproducible, so that independent parties
can validate that we really deploy what we claim to deploy.

We try to achieve some level of reproducibility using a Dockerized build
environment. The following steps _should_ build the official Wasm image

    ./scripts/docker-build
    sha256sum internet_identity.wasm

The resulting `internet_identity.wasm` is ready for deployment as
`rdmx6-jaaaa-aaaaa-aaadq-cai`, which is the reserved principal for this service.

Our CI also performs these steps; you can compare the SHA256 with the output there, or download the artifact there.



## Dependencies

- `dfx` version 0.8.3

- [`ic-cdk-optimizer`](https://github.com/dfinity/cdk-rs/tree/main/src/ic-cdk-optimizer) version 0.3.1

- Rust version 1.51

- NodeJS (with npm) version TBD

- CMake

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
echo "http://localhost:8000?canisterId=$(dfx canister id internet_identity)"
```

### Contributing to the frontend

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

Finally, to test workflows like authentication from a client application, you start the sample app:

```bash
cd demos/sample-javascript
npm run develop
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

We have a set of Selenium tests that run through the various flows. To set up a local deployment follow the steps in `.github/workflows/selenium.yml`.
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


### Contributing to the backend

The Internet Identity backend is a Wasm canister implemented in Rust and built from the `internet_identity` cargo package (`src/internet_identity`).
Some canister functionality lives in separate libraries that can also be built to native code to simplify testing, e.g., `src/cubehash`.

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

## Build flavors

The Internet Identity build can be customized to include "flavors" that are
useful when developing and testing.

⚠️ These options should only ever be used during development as they effectively poke security holes in Internet Identity ⚠️

These options can be used both in the "contributing" workflows above (frontend, backend) and
in the docker build. The flavors are enabled by setting the corresponding
environment variable to `1`. Any other string, as well as not setting the
environment variable, will disable the flavor.

For instance:

``` bash
$ II_FETCH_ROOT_KEY=1 dfx build
$ II_DUMMY_CAPTCHA=1 II_DUMMY_AUTH=1 ./scripts/docker-build
```

The flavors are described below:

| Environment variable | Description |
| --- | --- |
| `II_FETCH_ROOT_KEY` | When enabled, this instructs the frontend call to fetch the "root key" from the replica.<br/>The Internet Computer (https://ic0.app) uses a private key to sign responses. This private key not being available locally, the (local) replica generates its own. This option effectively tells the Internet Identity frontend to fetch the public key from the replica it connects to. When this option is _not_ enabled, the Internet Identity frontend code will use the (hard coded) public key of the Internet Computer. |
| `II_DUMMY_CAPTCHA` | When enabled, the CAPTCHA challenge (sent by the canister code to the frontend code) is always the known string `"a"`. This is useful for automated testing. |
| `II_DUMMY_AUTH` | When enabled, the frontend code will use a known, stable private key for registering anchors and authenticating. This means that all anchors will have the same public key(s). In particular this bypasses the WebAuthn flows (TouchID, Windows Hello, etc), which simplifies automated testing. |
