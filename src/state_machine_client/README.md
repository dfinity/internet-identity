# Test State Machine Client

This is a client library for the `test-state-machine` binary from the IC repository: https://github.com/dfinity/ic/tree/master/rs/state_machine_tests

Whenever a new `StateMachine` is created, the library spawns a child process, which it will communicate with using `stdio`. If the `debug` flag is passed in, the binary will print debug information to `stderr` (as to not interfere with the protocol going over `stdin/stdout`).

The `ic-test-state-machine` is an _incomplete_ wrapper around the `ic-state-machine-tests` library (in the same crate). It was created to decouple the lengthy build process and the many dependencies of the IC repo from clients of the `ic-test-state-machine`.

## Download

The prebuilt binary can be downloaded from https://download.dfinity.systems/ic/$sha/binaries/x86_64-$platform/ic-test-state-machine.gz, where `sha` is the commit sha of a commit on the master branch of the IC repository and `platform` is either `linux` or `darwin`.

## Dependencies

The `ic-test-state-machine` binary requires `openssl 3`. On MacOs it can be installed using homebrew by running `brew install openssl@3`.

## Disclaimer

While testing with the `ic-test-state-machine` might help the development process, it is not a replacement for testing with the actual replica.
