# Test App

This app is used by the Internet Identity Selenium tests. It contains additional functionality for manual testing and debugging that is not required to integrate with Internet Identity. See `demos/using-dev-build` for a minimal example on how to authenticate with II.

## Getting Started

1. Ensure all dependencies are installed: `npm ci`
2. Run the local replica `dfx start --clean`
3. Deploy the canister to the local replica `dfx deploy`
4. Visit the running site at http://localhost:8000?<canister_id>
   1. alternatively the dev server can be started by running `npm run develop` which can be accessed on http://localhost:8080

