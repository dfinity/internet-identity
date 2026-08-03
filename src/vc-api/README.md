# VC Issuer API

A small TypeScript library with type & [zod](https://zod.dev) definitions for the [issuer API](https://github.com/dfinity/internet-identity-vc-demo-issuer).

The types in `src/generated` are produced from `vc_demo_issuer.did`, a vendored
copy of the Candid interface published by the issuer release pinned in
[`.github/versions/vc-issuer`](../../.github/versions/vc-issuer).

## Usage

Build from the root directory:

```bash
$ npm run --workspace ./src/vc-api build
```
