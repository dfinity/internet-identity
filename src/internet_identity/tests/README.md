# Internet Identity Tests

This is a test suite for the Internet Identity canister. To run the tests:

``` bash
# Make sure II is built with the "test" flavor
II_FETCH_ROOT_KEY=1 II_DUMMY_CAPTCHA=1 ./scripts/build

# Build the archive canister
./scripts/build --archive

# Make sure you have a copy of the latest release of II
curl -SL https://github.com/dfinity/internet-identity/releases/latest/download/internet_identity_test.wasm -o internet_identity_previous.wasm

# Make sure you have a copy of the latest release of the archive
curl -SL https://github.com/dfinity/internet-identity/releases/latest/download/archive.wasm -o archive_previous.wasm

# Run the tests
cargo test -p internet_identity
```

_NOTE: you can also run the tests with `cargo test -p internet_identity --release` which takes much longer to build, but the tests are then orders of magnitude faster._
