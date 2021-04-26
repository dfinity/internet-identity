This is a convenience copy of the code in https://github.com/miracl/core
produced from revision fc6aca93238824fe84d649181773cba29b4fcb1e with these steps:

* `cd c/`
* `python config64.py`
* Select BLS12381 (enter 31, then enter 0)
* Copy the files listed in ic-ref.cabal to this directory
* … and then re-do the patches required to use ZCash-style compressed encoding
  of curve points, unless mircal has implemented
  https://github.com/miracl/core/issues/21 in the meantime :-(
* … and make sure the domain separator is `BLS_SIG_BLS12381G1_XMD:SHA-256_SSWU_RO_NUL_`
* … and do not use the “new multi-pairing mechanism”, but the alternative in `bls_BLS12381.c`

Yes, this is not the “right” way of doing it, but keeps `ic-ref` self-contained, which is useful especially when building it without nix (local development with cabal; the agent-rust CI integration).

If you get linking errors with `cabal repl ic-ref-test`, try tweaking the order of the `c-sources` in `ic-ref.cabal`.
