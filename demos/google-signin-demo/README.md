# Google Sign-In Demo App

This is a demo app that follows the template from `demos/test-app`. It features a single "Sign in with Google" button that does nothing.

## Development

1. Ensure all dependencies are installed from the root: `npm install`
2. Run the local replica `dfx start --clean`
3. Deploy the canister to the local replica `dfx deploy` (you may need to create a `dfx.json` from `dfx.example.json`)
4. Visit the running site at `http://localhost:4943?canisterId=<canister_id>`
   1. alternatively the dev server can be started by running `npm run dev --workspace ./demos/google-signin-demo`
