# sig-verifier-js

JS/TS Wrapper for verifying various signatures.

> [!WARNING]
> This library is under development and is untested. This library should not be used.

Supported functionality:

- `verify_basic_sig_by_public_key(message, signature, public_key)`: Verifies a basic (i.e. not a canister signature)
  [IC supported signature](https://internetcomputer.org/docs/current/references/ic-interface-spec/#signatures).
- `verify_canister_sig(message, signature, public_key, ic_root_public_key)`: Verifies an
  [IC canister signature](https://internetcomputer.org/docs/current/references/ic-interface-spec/#canister-signatures).
- `verify_ic_signature(message, signature, public_key, ic_root_public_key)`: Verifies any
  [IC supported signature](https://internetcomputer.org/docs/current/references/ic-interface-spec/#signatures).
- `validate_delegation_and_get_principal(challenge, signed_delegation_chain_json,
current_time_ns, ii_canister_id, ic_root_public_key_raw)`:
  Verifies the validity of the given signed delegation chain
  wrt. the challenge, and the other parameters. Specifically, it checks that:
  - `signed_delegation_chain` contains exactly one delegation, denoted below as `delegations[0]`
  - `delegations[0].pubkey` equals `challenge` (i.e. challenge is the "session key")
  - `signed_delegation_chain.publicKey` is a public key for canister signatures of `ii_canister_id`
  - `current_time_ns` denotes point in time before `delegations[0].expiration`
  - `delegations[0].signature` is a valid canister signature on a representation-independent hash of `delegations[0]`,
    wrt. `signed_delegation_chain.publicKey` and `ic_root_public_key_raw`
    Upon successful verification returns a self-authenticating Principal derived
    from `signed_delegation_chain.publicKey`.

## Build

From the root of the repository:

```
$ npm run --workspace ./src/sig-verifier-js build
$ npm run --workspace ./src/sig-verifier-js test
```
