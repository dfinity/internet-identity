# An example issuer of verifiable credentials

This is an example implementation of an issuer of verifiable credentials for
the [attribute sharing flow on the IC](https://github.com/dfinity/wg-identity-authentication/blob/main/topics/attribute-sharing.md).

## Supported Behavior

The app implements the interface of the issuer from [the spec](../../docs/vc-spec.md), plus some additional
APIs for configuring and testing, cf. [vc_issuer.did](./vc_issuer.did).
Please note that this issuer is for demonstrating the use of issuer APIS, and is **not meant
for real-world deployment**, as it does not have proper management of user data.

## Development

Run `./build.sh` script to build the issuer canister.
