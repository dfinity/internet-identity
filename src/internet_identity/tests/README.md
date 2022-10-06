# Internet Identity Tests

This is a test suite for the Internet Identity canister. To run the tests:

``` bash
# Make sure II is built with the "test" flavor
II_DUMMY_CAPTHA=1 ./scripts/build

# Make sure you have a copy of the latest release of II
curl -SL https://github.com/dfinity/internet-identity/releases/latest/download/internet_identity_test.wasm -o internet_identity_previous.wasm

# Run the tests
cargo test -p internet_identity
```

_NOTE: you can also run the tests with `cargo test -p internet_identity --release` which takes much longer to build, but the tests are then orders of magnitude faster._
