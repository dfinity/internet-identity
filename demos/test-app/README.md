# Test App

This app is used by the Internet Identity Selenium tests. It contains additional functionality for manual testing and debugging that is not required to integrate with Internet Identity. See `demos/using-dev-build` for a minimal example on how to authenticate with II.

## Supported Behavior
* Authenticate using a customizable instance of II 
  * using `agent-js`
  * using a custom implementation which gives full control over the window post messages being sent
  * support for customizing
    * max time to live
    * derivation origin
  * After successful authentication the full delegation is displayed.
* Support for the asset `/.well-known/ii-alternative-origins` to test implementations of the `derivationOrigin` validation in II
  * The asset can be customized to be:
    * Valid as per [spec](../../docs/internet-identity-spec.adoc)
      * including the requested `derivationOrigin`
      * not including the requested `derivationOrigin`
    * Invalid
      * Wrong format
      * Missing certification
      * Respond with redirect

## Development
Note: These steps are intended to do development on the test-app. To simply run selenium tests against the test-app follow [these instructions](../../docker-test-env/README.md) instead. 
1. Ensure all dependencies are installed: `npm ci`
2. Run the local replica `dfx start --clean`
3. Deploy the canister to the local replica `dfx deploy`
4. Visit the running site at http://localhost:4943?<canister_id>
   1. alternatively the dev server can be started by running `npm run start` which can be accessed on http://localhost:8080

