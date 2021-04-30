The IC reference implementation
===============================

`ic-ref` is a partial implementation of the external interface of the DFINITY
Internet Computer, as specified in the [Interface Spec].

[Interface Spec]: https://docs.dfinity.systems/public/

Goals
-----

The goals of the reference implementation are

 * It evolves in lock-step with the Interface Spec. At least versioned releases of
   the Interface Spec come with a complete implementation of `ic-ref`.

 * Supplement the prose and pseudo-code in the Interface Spec for additional and
   concrete clarity.

 * Ideally, relevant code pieces of `ic-ref` are as easy to understand as
   carefully written pseudo-code.

 * Increase weight of and confidence in the Interface Spec, by demonstrating
   implementability.

 * Aid in the development of the Interface Spec by uncovering omissions,
   inconsistencies or unexpected complexity.

 * Allow testing of external clients (like `dfx`) directly against the
   reference implementation.

 * Provide a simplified mock environment for testing Canisters (e.g. `motoko`
   output) that does not require networking.

   In particular, provide `ic-ref-run`, a binary that allows to script the
   execution of a single canister.

 * Aid in the production implementation of the Internet Computer, by allowing
   to probe the reference implementation to better understand intended
   behaviour and by comparing the behaviour of the two.

 * Performance is good enough to run small examples.

 * The primary focus is describing the happy path execution, and not
   necessarily the precise error reporting behaviour upon bad usage (e.g. bad
   canisters, resource exhaustion, module validation).

Additionally, we want `ic-ref` to be a useful tool for Canister developers to
run their canisters locally. This adds additional goals:

 * The state of `ic-ref` can be persisted and picked up later.

 * Debugging/logging/trace features that aid understanding the behaviour and/or
   help debug canisters.

Should these goals eventually conflict with the goals for a reference
implementation, e.g. becauese they impose complexity that is not easy to contain in auxillary modules, a project split might be considered.

There are also explicit non-goals to keep in mind:

 * `ic-ref` does not need to support canisters that are large or very
   long-running.

 * No persistence across different versions of `ic-ref`.

 * It is explicitly not desirable to share code between reference and
   production implementation, to achieve the multi-version cross-checking
   effect. Not using Rust for `ic-ref` helps with that.

 * No guaranteed protection against bad effects from malicious interactions.

 * No duplication of commonly available functionality. In particular, the
   assumption is that the production implementation will use a mature Wasm
   embedder that implements Wasm validation correctly, so `ic-ref` does not
   itself implement validation.

Furthermore there are some stretch goals that would be nice to have, but not if
requires compromising the main goals.

 * The reference implementation describes _one_ possible execution, but not
   _all_ possible behaviours of the Interface Spec. If this can be changed (e.g.
   using non-deterministic modeling of computation) without compromising
   readability and normal execution, then this would be nice.

 * A deep or type-level embedding of the interfaces (HTTP, System) that can be
   used separately to  generation of production code (“interface stubs”).

 * It could serve as a starting point for applying formal verification to this
   part of the system, e.g. by converting the (non-plumbing) modules to Coq
   using `hs-to-coq`, or by implementing them in a theorem prover and
   extracting Haskell code from it.

To achieve these goals, the following design decisions are made:

 * `ic-ref` is implemented in Haskell, to optimize for development speed,
   type-checking safety, and readablity of carefully selected portions of the
   code.

 * As far as possible, a module either

   - corresponds closely to the spec, and is written with readability as a high
     priority, avoiding language features that obscure meaning. The rough goal
     is “executable pseudo-code”. The use of advanced langauge features or non-idiomatic
     code that _help_ readability are encouraged.

     Examples: `IC.Ref`, `IC.Canister.Impl`, `IC.HTTP.RequestId`

   - is a plumbing module that handles some technical aspect, and pave the way
     for the simplicity in the previously mentioned modules. It is expected
     that reading such modules may require high level of familiarity with Haskell.

     Examples: `IC.Wasm.Imports`, `IC.HTTP.CBOR`.

   This is an ongoing refinement process, striving for a probably unattainable
   ideal as the goal.

Running
-------

This is the fastest way to run `ic-ref` or `ic-ref-test` is to use the
following commands in the `impl/` directory:

    nix run -f . -c ic-ref
    nix run -f . -c ic-ref-test

You can also pass arguments, e.g.

    nix run -f . -c ic-ref-test --endpoint http://0.0.0.0:8080 -p 'WebAuthn'

Using
-----

* The `ic-ref` program starts a webserver at `http://0.0.0.0:8001/` that implements the
  Internet Computer interface, and can be used with `dfx --client http://0.0.0.0:8001/`.

  If you point your browser to `http://0.0.0.0:8001/` you get the evolution of
  the IC state as JSON. Recommended to use Firefox, as it provides a nice UI for
  browsing JSON.

  If the `--state-file FILE` argument is given, `ic-ref` will persist its state
  in this file. Note that if that file cannot be read (e.g. because it is from
  an incompatible version of `ic-ref`), starting `ic-ref` will fail.

* The `ic-ref-test` acceptance test.

  Pass `--endpoint http://localhost:8080/` to run against a specific node.

  With the `-p pattern` flag you can select individual tests; those whose names
  contain the pattern. See https://github.com/feuerbach/tasty#patterns for
  advanced use of this flag.

  When passing `--rerun`, the test suite will remember which tests have failed,
  and only run those that failed last tests (or all again, if none have failed
  last run).

* The `ic-request-id` tool takes a CBOR-request (stdin or via a file) and
  calculates its request id.

* The `ic-ref-run` program takes, as an argument, a file with `install`, `call`,
  `query`, `upgrade` commands, just like
  [`drun`](https://github.com/dfinity-lab/dfinity/tree/master/rs/drun/).

The `--version` flag reports the current version.

Interactive use of ic-ref-test
------------------------------

You can actually use the Haskell REPL to interact with the internet computer:
```
~/dfinity/ic-ref/impl $ cabal repl ic-ref-test
…
Ok, 27 modules loaded.
*Main> :m + *IC.Test.Spec
*IC.Test.Spec *Main> :set -XOverloadedStrings
*IC.Test.Spec *Main> R r <- connect "http://localhost:34677/"
Fetching endpoint status from "http://localhost:34677"...
Spec version tested:  0.14.0
Spec version claimed: 0.14.0
*IC.Test.Spec *Main> r $ install noop
"\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\SOH\SOH"
*IC.Test.Spec *Main> cid1 <- r $ install noop
*IC.Test.Spec *Main> prettyBlob cid1
"00000000000000020101"
*IC.Test.Spec *Main> r $ call cid1 (setGlobal "Foo" >>> replyData "Hello")
"Hello"
*IC.Test.Spec *Main> r $ query cid1 (replyData getGlobal)
"Foo"
```

It’s necessary to wrap all lines with the `r $ …` for now; this sets the
endpoint parameter.

Developing on ic-ref
---------------------

Running `nix-shell` in the `ic-ref/impl` directory gives you an environment
that allows you to build the project using `cabal build`. You can also run
`cabal run ic-ref` etc. to run it directly from source.

One possible workflow is to run

    ghcid -c 'cabal repl ic-ref' -T Main.main

which will run `ic-ref` and restart upon file changes.  Similarly

Developing on ic-ref-test
-------------------------

Before running the test suite, make sure you have built the universal canister.
The symbolic link in `impl/test-data/universal_canister.wasm` points to the
build output produced by

    cd ../universal_canister
    nix-shell
    cargo build --target wasm32-unknown-unknown --release

now you can run the test suite from the `impl/` directory with

    cabal run ic-ref-test

The `-p` flag, i.e.

    cabal run ic-ref-test -- -p upgrade

allows you can run tests selectively (i.e. only those whose name include
“upgrade”).

Again, you can use `ghcid` to run the test suite upon file changes:

    ghcid -c 'cabal repl ic-ref-test' -T Main.main

and you can flags with

    ghcid -c 'cabal repl ic-ref-test' -T Main.main --setup ':set args --rerun -p "query call"'


Updating Haskell Packages
-------------------------

When the `.cabal` file of a Haskell package is changed you need to make sure the
corresponding nix files `nix/generated/` are kept in sync with it. These are
automatically generate, run

    nix-shell nix/generate.nix

to update.

Don't worry if you forget to update the `default.nix` file, the CI job
`check-generated` checks if these files are in sync and fails with a diff if
they aren't.
