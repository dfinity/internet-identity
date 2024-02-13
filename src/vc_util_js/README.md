# vc-util-web

JS/TS Wrapper for the [`vc_util` Rust crate](../vc_util).


> [!WARNING]
> There are some caveats to using this library:
> 1. This library only supports verifying a specific kind of credentials (age).
> 1. **Verifying signatures in the front-end is generally unsafe!** Malicious users could modify the front-end code and bypass the signature verification. This library is intended for use in demos, prototypes, backends and other situations where the code either cannot be tampered with _or_ the tampering does not pose a security risk.
> 1. This library is in alpha version. This means that the API may change at any time.
> 1. The resulting wasm module is _heavy_, around 1MB gzipped. Do not use this library if you are concerned about the size of your bundle.


## Prerequisites

- [rust](https://www.rust-lang.org)
- [wasm-pack](https://github.com/rustwasm/wasm-pack)

## Building

Run the following command
```bash
$ npm run build
```
## Usage Example

```js
import vcUtil, {validateVerifiedAdultPresentation} from "@dfinity/vc-util-web";
export var ROOT_PUBLIC_KEY_RAW = new Uint8Array([
    0x81, 0x4c, 0x0e, 0x6e, 0xc7, 0x1f, 0xab, 0x58, 0x3b, 0x08, 0xbd, 0x81, 0x37, 0x3c, 0x25, 0x5c, 0x3c,
    0x37, 0x1b, 0x2e, 0x84, 0x86, 0x3c, 0x98, 0xa4, 0xf1, 0xe0, 0x8b, 0x74, 0x23, 0x5d, 0x14, 0xfb, 0x5d,
    0x9c, 0x0c, 0xd5, 0x46, 0xd9, 0x68, 0x5f, 0x91, 0x3a, 0x0c, 0x0b, 0x2c, 0xc5, 0x34, 0x15, 0x83, 0xbf,
    0x4b, 0x43, 0x92, 0xe4, 0x67, 0xdb, 0x96, 0xd6, 0x5b, 0x9b, 0xb4, 0xcb, 0x71, 0x71, 0x12, 0xf8, 0x47,
    0x2e, 0x0d, 0x5a, 0x4d, 0x14, 0x50, 0x5f, 0xfd, 0x74, 0x84, 0xb0, 0x12, 0x91, 0x09, 0x1c, 0x5f, 0x87,
    0xb9, 0x88, 0x83, 0x46, 0x3f, 0x98, 0x09, 0x1a, 0x0b, 0xaa, 0xae]);

async function verifyAdultVp(vpJwt : string, vcSubjectPrincipal: Principal, issuerOrigin: string,
                             iiCanisterId: Principal, issuerCanisterId: Principal) {
    await vcUtil();
    const currentTimeNs = new Date().getTime().valueOf();
    var encoder = new TextEncoder();
    try {
        validateVerifiedAdultPresentation(encoder.encode(vpJwt),
            vcSubjectPrincipal.toUint8Array(),
            iiCanisterId.toUint8Array(),
            encoder.encode(issuerOrigin),
            issuerCanisterId.toUint8Array(),
            ROOT_PUBLIC_KEY_RAW,
            BigInt(currentTimeNs))
        console.log('--- Adult VP verified successfully')
    } catch (error) {
        console.error('--- Adult VP verification failed', error)
    }
}
```
