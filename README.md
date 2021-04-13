# idp_service

## Requirements

### Software

`dfx` version 0.7.0-beta.2 or later
Rust version TBD
NodeJS (with npm) version TBD

## Running Locally

To run the idp_service canisters, proceed as follows after cloning the repository

```bash
npm install
dfx start [--clean] [--background]
dfx deploy
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
npm install
dfx start [--clean] [--background]
dfx deploy
```

Then, run `npm start` to start webpack-dev-server.

Unit tests can be run with `npm run test`. To run tests throughout your development cycle, run `npm run test -- --watchAll` or use [wallaby.js](https://wallabyjs.com/) as a test-runner.