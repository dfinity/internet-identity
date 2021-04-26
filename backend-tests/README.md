Backend blackbox testing
========================

This directory contains a test suite that tests the backend of the Internet
Identity in a simulated IC environment.

The subdirectory `ic-ref` contains a snapshopt of the repository at
https://github.com/dfinity-lab/ic-ref, created using `git subtree`. This is
unmodified, so that changes there can be merged in more easily.

We do not use the `.cabal` file there, but have our own in
`backend-tests.cabal`, defining just a single executable.


Setup
-----

 * Install ghc-8.8.4 and `cabal` (e.g. using https://www.haskell.org/ghcup/)
 * Run `cabal build`

Running
-------

 * Build the top-level directory, build the backend canister (`dfx build idp_service`)
 * In the present directory, run
   ```
   cabal run -v0 backend-tests --
   ```

Options
-------

By default, this tests the wasm file in

    ../target/wasm32-unknown-unknown/release/idp_service.wasm

to use a different one, pass the `--wasm` flag to `backend-tests`

You can select tests to run using `-p`, e.g.

    cabal run -v0 backend-tests -- -p 'get multiple delegations and expire'

See `--help` for more options.

Developing
----------

The simplest way to get a good developer experience is running `ghcid`

    ghcid -c 'cabal repl backend-tests'

while editing, and saving the code to reload in `ghci`. You can also use

    ghcid -c 'cabal repl backend-tests' -T Main.main --setup ":set args -p \"delegation\""

to run some tests after each save, for a quick development iteration
