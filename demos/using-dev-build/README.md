# Integrating with Internet Identity

This shows how to integrate and test a project with Internet Identity. This uses the development [build flavor](https://github.com/dfinity/internet-identity/blob/main/README.md#build-features-and-flavors).

## Running the tests

The following commands will start a replica, install the development Internet Identity canister, and run the test suite:

```bash
$ dfx start # in a different terminal
$ dfx deploy --no-wallet
$ npm ci
$ npm run test
```

For more information, check the `dfx.json` file and the [SDK documentation](https://smartcontracts.org/docs/quickstart/quickstart-intro.html) for reference.
