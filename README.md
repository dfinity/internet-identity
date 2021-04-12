# idp_service

## Requirements

### Software

* `dfx` version from the branch at https://github.com/dfinity/sdk/pull/1587.

  One easy way to fetch it built from the nix cache is to run
  ```
  nix-build -E '(import (builtins.fetchGit { url = "git@github.com:dfinity/sdk"; ref = "joachim/idp";}) {}).dfx.standalone' -o /tmp/idp-dfx
  ```
  and then run it as `/tmp/idp-dfx/bin/dfx`:
  ```
  /tmp/idp-dfx/bin/dfx --version
  dfx 0.7.0-beta.2.idp
  ```
  After upgrading it may help to run
  ```
  /tmp/idp-dfx/bin/dfx cache delete --help
  ```

* Rust version 1.50

* NodeJS (with npm) version TBD

## Running Locally

- [Quick Start](https://sdk.dfinity.org/docs/quickstart/quickstart-intro.html)
- [SDK Developer Tools](https://sdk.dfinity.org/docs/developers-guide/sdk-guide.html)
- [Motoko Programming Language Guide](https://sdk.dfinity.org/docs/language-guide/motoko.html)
- [Motoko Language Quick Reference](https://sdk.dfinity.org/docs/language-guide/language-manual.html)

To run the idp_service canisters, proceed as follows after cloning the repository

```bash
npm install
dfx start [--clean] [--background]
dfx deploy
```

Then the canister can be used as

```bash
dfx canister call idp_service register '(123, "test", {1; 2; 3}; null)'
```

To open the front-end, you can run the following and open the URL.
```bash
echo "http://localhost:8000?canisterId=$(dfx canister id frontend)"
```
