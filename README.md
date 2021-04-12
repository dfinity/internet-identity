# idp_service

## Requirements

### Software
`dfx` version 0.7.0-beta.2 or later
Rust version TBD
NodeJS (with npm) version TBD

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
