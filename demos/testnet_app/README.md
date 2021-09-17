# testnet_app

A minimal app that can be used to test that II is working on a given testnet.

It consists of a whoami Motoko backend and a small JS frontend served by an asset canister.

To deploy this app to the II testnet run the following commands from the II project root:
```bash
cd demos/testnet_app/
npm ci
dfx deploy --network identity
```
