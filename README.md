# idp_service

## Requirements

### Software

- `dfx` version from the branch at https://github.com/dfinity/sdk/pull/1587.

  The easiest way to run this is to just use

  ```
  ./dfx.sh
  ```

  One other easy way to fetch it built from the nix cache is to run

  ```
  nix-build -E '(import (builtins.fetchGit { url = "git@github.com:dfinity/sdk"; ref = "joachim/idp";}) {}).dfx.standalone' -o /tmp/idp-dfx
  ```

  and then run it as `/tmp/idp-dfx/bin/dfx`:

  ```
  /tmp/idp-dfx/bin/dfx --version
  dfx 0.7.0-beta.2.idp
  ```

  After upgrading it may help to run

  ```
  /tmp/idp-dfx/bin/dfx cache delete --help
  ```

- Rust version 1.50

- NodeJS (with npm) version TBD

## Running Locally

To run the idp_service canisters, proceed as follows after cloning the repository

```bash
npm install
dfx start [--clean] [--background]
dfx deploy --no-wallet --argument '(null)'
```

Then the canister can be used as

```bash
dfx canister call idp_service register '(123, "test", vec {1; 2; 3}, null)'
```

To open the front-end, you can run the following and open the URL.

```bash
echo "http://localhost:8000?canisterId=$(dfx canister id frontend)"
```

### Contributing to the frontend

We are practicing TDD for functional requirements for this project. For your ticket, either find an open spec under `src/frontend/src/__tests__` or create a new unit test to cover your functionality.

Mocking and stubbing is recommended for Unit tests, as long as you make sure the returned types match the candid definitions.

The fastest workflow to get the development environment running is to deploy once with

```bash
npm ci
dfx start [--clean] [--background]
dfx deploy --no-wallet --argument '(null)'

```

Then, run `CANISTER_ID=$(dfx canister id idp_service) npm start` to start webpack-dev-server.

Unit tests are tbd. They can be run with `npm run test`. To run tests throughout your development cycle, run `npm run test -- --watchAll` or use [wallaby.js](https://wallabyjs.com/) as a test-runner. Frontend tests are not required to pass at this time.

To customize your canister ID for deployment or particular local development, create a `.env` file in the root of the project and add a `CANISTER_ID` attribute. It should look something like

```
CANISTER_ID=rrkah-fqaaa-aaaaa-aaaaq-cai
```

### Contributing to the backend

The Internet Identity backend is a Wasm canister implemented in Rust and built from the `idp_service` cargo package (`src/idp_service`).
Some canister functionality lives in separate libraries that can also be built to native code to simplify testing, e.g., `src/certified_map`, `src/hashtree`, `src/cubehash`, etc.

Run the following command in the root of the repository to execute the test suites of all the libraries:

```bash
cargo test
```

The backed canister is also used to serve the frontend assets.
This creates a dependency between the frontend and the backend.
So running the usual `cargo build --target wasm32-unknown-unknow -p idp_service` might not work or include an outdated version of the frontend.

Use the following command to build the backend canister Wasm file instead:

```bash
dfx build idp_service
```

The Wasm file will be located at `target/wasm32-unknown-unknown/release/idp_service.wasm`.
