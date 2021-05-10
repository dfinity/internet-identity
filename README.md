# Internet Identity Service

See `./docs/ic-idp-spec.adoc` for a details specification and technical
documentation.

## Official build

The official build should ideally be reproducible, so that independent parties
can validate that we really deploy what we claim to deploy.

We try to achieve some level of reproducibility using a Dockerized build
environment. The following steps _should_ build the official Wasm image

    docker build -t internet-identity-service .
    docker run --rm --entrypoint cat internet-identity-service /internet_identity.wasm > internet_identity.wasm
    sha256sum internet_identity.wasm

The resulting `internet_identity.wasm` is ready for deployment as
`rdmx6-jaaaa-aaaaa-aaadq-cai`, which is the reserved principal for this service.

Our CI also performs these steps; you can compare the SHA256 with the output there, or download the artifact there.



## Software versions

- `dfx` version 0.7.0-beta.6

- Rust version 1.51

- NodeJS (with npm) version TBD

## Running Locally

To run the internet_identity canisters, proceed as follows after cloning the repository

```bash
npm install
dfx start [--clean] [--background]
dfx deploy --no-wallet --argument '(null)'
```

Then the canister can be used as

```bash
dfx canister call internet_identity register '(123, "test", vec {1; 2; 3}, null)'
```

To open the front-end, you can run the following and open the URL.

```bash
echo "http://localhost:8000?canisterId=$(dfx canister id frontend)"
```

### Contributing to the frontend

The fastest workflow to get the development environment running is to deploy once with

```bash
npm ci
dfx start [--clean] [--background]
dfx deploy --no-wallet --argument '(null)'
```

Then, run `CANISTER_ID=$(dfx canister id internet_identity) npm start` to start webpack-dev-server.

To customize your canister ID for deployment or particular local development, create a `.env` file in the root of the project and add a `CANISTER_ID` attribute. It should look something like
```
CANISTER_ID=rrkah-fqaaa-aaaaa-aaaaq-cai
```

We have a set of Selenium tests that run through the various flows. To run them locally follow the steps in `.github/workflows/selenium.yml`.

We autoformat our code using `prettier`. Running `npm run format` formats all files in the frontend.
If you open a PR that isn't formatted according to `prettier`, CI will automatically add a formatting commit to your PR.

We use `eslint` to check the frontend code. You can run it with `npm run lint`, or set up your editor to do it for you.


### Contributing to the backend

The Internet Identity backend is a Wasm canister implemented in Rust and built from the `internet_identity` cargo package (`src/internet_identity`).
Some canister functionality lives in separate libraries that can also be built to native code to simplify testing, e.g., `src/certified_map`, `src/hashtree`, `src/cubehash`, etc.

Run the following command in the root of the repository to execute the test suites of all the libraries:

```bash
cargo test
```

The backed canister is also used to serve the frontend assets.
This creates a dependency between the frontend and the backend.
So running the usual `cargo build --target wasm32-unknown-unknow -p internet_identity` might not work or include an outdated version of the frontend.

Use the following command to build the backend canister Wasm file instead:

```bash
dfx build internet_identity
```

The Wasm file will be located at `target/wasm32-unknown-unknown/release/internet_identity.wasm`.
