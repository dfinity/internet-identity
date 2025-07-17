# An example issuer of verifiable credentials

This is an example implementation of an issuer of verifiable credentials for
the [attribute sharing flow on the IC](https://github.com/dfinity/wg-identity-authentication/blob/main/topics/attribute-sharing.md).

## Supported Behavior

The app implements the interface of the issuer from [the spec](../../docs/vc-spec.md), plus some additional
APIs for configuring and testing, cf. [vc_demo_issuer.did](./vc_demo_issuer.did). These additional APIs
enable "self-registration" of users, so that the issuer can issue credentials for them.
The issuer also offers a simple FE component, that allows for "self-registration" via a browser.

**WARNING:** Please note that this issuer is for demonstrating the use of issuer APIs, and is **not meant
for real-world deployment**, as it does not have proper management of user data.

## Development and Testing

Run `./build.sh` script to build the issuer canister.

An up-to-date binary of the issuer (`vc_demo_issuer.wasm.gz`) is built during the release
process and the latest binary can be fetched via [release notes page](https://github.com/dfinity/internet-identity/releases/latest).

To use the demo issuer in a testing setup, the issuer canister has to be configured to match the environment,
i.e. it has to use the correct root public key, and accept requests from the correct II-canister
(see [`provision`-script](./provision) for details, and [HACKING-doc](../../HACKING.md) for setup instructions).
