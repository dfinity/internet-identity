<p align="center"><a href="https://identity.ic0.app" target="_blank" rel="noopener noreferrer"><img width="600" src="./ii-logo.png" alt="Internet Identity"></a></p>

<p align="center">
    <a href="https://github.com/dfinity/internet-identity/actions/workflows/canister-tests.yml"><img src="https://github.com/dfinity/internet-identity/actions/workflows/canister-tests.yml/badge.svg" alt="Canister Tests"></a>
    <a href="https://github.com/dfinity/internet-identity/actions/workflows/rust.yml"><img src="https://github.com/dfinity/internet-identity/actions/workflows/rust.yml/badge.svg" alt="Rust"></a>
    <a href="https://github.com/dfinity/internet-identity/actions/workflows/frontend-checks.yml"><img src="https://github.com/dfinity/internet-identity/actions/workflows/frontend-checks.yml/badge.svg" alt="Frontend checks and lints"></a>
    <a href="https://github.com/dfinity/internet-identity/releases"><img src="https://img.shields.io/github/downloads/dfinity/internet-identity/total?label=downloads&logo=github" alt="GitHub all releases"></a>
</p>

<p align="center">
    🔗 <a href="https://identity.ic0.app">https://identity.ic0.app</a> • 📜 <a href="https://internetcomputer.org/docs/current/references/ii-spec">Specification</a> <br/> ― <br/>📚 <a href="https://forum.dfinity.org/c/internet-identity/32">Forum</a> • 🚑 <a href="https://github.com/dfinity/internet-identity/issues/new">Report an Issue</a> • 📞 <a href="https://discord.gg/E9FxceAg2j">Discord</a>
</p>

---
<p style="text-align: center;color:red">#######################################################</p>

**WARNING**: this is a feature branch for Verifiable Credentials MVP (cf. [Working Group](https://github.com/dfinity/wg-identity-authentication/blob/main/topics/attribute-sharing.md)).  The code in this branch is experimental and can change at any time.
For the current implementation of the Internet Identity please see the `main` branch
<p style="text-align: center;color:red">#######################################################</p>

Internet Identity is an authentication service for the [Internet Computer][ic]. It is the authentication system that allows hundreds of thousands of users to log in to Dapps like [Distrikt], [DSCVR] and more.

Internet Identity is:

* **Simple**: It uses some of the [WebAuthn] API to allow users to register and authenticate without passwords, using TouchID, FaceID, Windows Hello, and more.
* **Flexible**: Integrating Internet Identity in a Dapp (or even Web 2 app) is as simple as opening the Internet Identity's HTTP interface, https://identity.ic0.app, in a new tab. No need to interact with the canister smart contract directly.
* **Secure**: Different identities are issued for each app a user authenticates to and cannot be linked back to the user.

For more information, see [What is Internet Identity?](https://internetcomputer.org/docs/current/tokenomics/identity-auth/what-is-ic-identity) on [internetcomputer.org](https://internetcomputer.org).

### Table of Contents

* [Getting Started](#getting-started)
  * [Architecture Overview](#architecture-overview)
  * [Building with Docker](#building-with-docker)
  * [Integration with Internet Identity](#integration-with-internet-identity)
* [Build Features and Flavors](#build-features-and-flavors)
  * [Features](#features)
  * [Flavors](#flavors)
* [Stable Memory Compatibility](#stable-memory-compatibility)
* [Getting Help](#getting-help)
* [Links](#links)

## Getting Started

This section gives an overview of Internet Identity's architecture, instructions on how to build the Wasm module (canister), and finally pointers for integrating Internet Identity in your own applications.

### Architecture overview

Internet Identity is an authentication service for the [Internet Computer][ic]. All programs on the Internet Computer are Wasm modules, or canisters (canister smart contracts).

![Architecture](./ii-architecture.png) <!-- this is an excalidraw.com image, source is ii-architecture.excalidraw -->

Internet Identity runs as a single canister which both serves the frontend application code, and handles the requests sent by the frontend application code.

> 💡 The canister (backend) interface is specified by the [internet_identity.did](./src/internet_identity/internet_identity.did) [candid] interface. The (backend) canister code is located in [`src/internet_identity`](./src/internet_identity), and the frontend application code (served by the canister through the `http_request` method) is located in [`src/frontend`](./src/frontend).

The Internet Identity authentication service works indirectly by issuing "delegations" on the user's behalf; basically attestations signed with some private cryptographic material owned by the user. The private cryptographic material never leaves the user's device. The Internet Identity frontend application uses the [WebAuthn] API to first create the private cryptographic material, and then the [WebAuthn] API is used again to sign delegations.

For information on how Internet Identity works in more detail, please refer to the following:

* [Internet Identity presentation 📼](https://youtu.be/oxEr8UzGeBo), streamed during the Genesis Event
* [Internet Identity Specification][spec], the official Internet Identity Specification

### Building with Docker

To get the canister (Wasm module) for Internet Identity, you can either **download a release** from the [releases] page, or build the code yourself. The simplest way to build the code yourself is to use [Docker] and the [`docker-build`](./scripts/docker-build) script:

``` bash
$ ./scripts/docker-build
```

The [`Dockerfile`](./Dockerfile) specifies build instructions for Internet Identity. Building the `Dockerfile` will result in a scratch container that contains the Wasm module at `/internet_identity.wasm.gz`.

> 💡 The build can be customized with [build features](#build-features-and-flavors).

We recommend using the [`docker-build`](./scripts/docker-build) script. It simplifies the usage of [build features](#build-features-and-flavors) and extracts the Wasm module from the final scratch container.

> 💡 You can find instructions for building the code without Docker in the [HACKING] document.

### Integration with Internet Identity

The [`using-dev-build`](./demos/using-dev-build) demo shows a documented example project that integrates Internet Identity. For more, please refer to the [Client Authentication Protocol section](https://internetcomputer.org/docs/current/references/ii-spec#client-authentication-protocol) of the [Internet Identity Specification][spec] to integration Internet Identity in your app from scratch. For a just-add-water approach using the [agent-js](https://github.com/dfinity/agent-js) library (also used by `using-dev-build`), check out Kyle Peacock's [blogpost](http://kyle-peacock.com/blog/dfinity/integrating-internet-identity/).

If you're interested in the infrastructure of how to get the Internet Identity canister and how to test it within your app, check out [`using-dev-build`](./demos/using-dev-build), which uses the Internet Identity development canister.

## Build Features and Flavors

The Internet Identity build can be customized to include [features](#features) that are
useful when developing and testing. We provide pre-built [flavors](#flavors)
of Internet Identity that include different sets of features.

### Features

These options can be used both when building [with docker](#building-with-docker) and
[without docker][HACKING]. The features are enabled by setting the corresponding
environment variable to `1`. Any other string, as well as not setting the
environment variable, will disable the feature.

For instance:

``` bash
$ II_FETCH_ROOT_KEY=1 dfx build
$ II_DUMMY_CAPTCHA=1 II_DUMMY_AUTH=1 ./scripts/docker-build
```

⚠️ These options should only ever be used during development as they effectively poke security holes in Internet Identity

The features are described below:

<!-- NOTE: If you add a feature here, add it to 'features.ts' in the frontend
codebase too, even if the feature only impacts the canister code and not the
frontend. -->

| Environment variable | Description |
| --- | --- |
| `II_FETCH_ROOT_KEY` | When enabled, this instructs the frontend code to fetch the "root key" from the replica.<br/>The Internet Computer (https://ic0.app) uses a private key to sign responses. This private key not being available locally, the (local) replica generates its own. This option effectively tells the Internet Identity frontend to fetch the public key from the replica it connects to. When this option is _not_ enabled, the Internet Identity frontend code will use the (hard coded) public key of the Internet Computer. |
| `II_DUMMY_CAPTCHA` | When enabled, the CAPTCHA challenge (sent by the canister code to the frontend code) is always the known string `"a"`. This is useful for automated testing. |
| `II_DUMMY_AUTH` | When enabled, the frontend code will use a known, stable private key for registering anchors and authenticating. This means that all anchors will have the same public key(s). In particular this bypasses the WebAuthn flows (TouchID, Windows Hello, etc), which simplifies automated testing. |
| `II_INSECURE_REQUESTS` | When enabled, the 'upgrade-insecure-requests' directive is removed from the content security policy in order to allow local development with Safari. |

### Flavors

We offer some pre-built Wasm modules that contain flavors, i.e. sets of features targetting a particular use case. Flavors can be downloaded from the table below for the latest release or from the [release page](https://github.com/dfinity/internet-identity/releases) for a particular release.

| Flavor | Description | |
| --- | --- | :---: |
| Production | This is the production build deployed to https://identity.ic0.app. Includes none of the build features. | [💾](https://github.com/dfinity/internet-identity/releases/latest/download/internet_identity_production.wasm) |
| Test | This flavor is used by Internet Identity's test suite. It fully supports authentication but uses a known CAPTCHA value for test automation. Includes the following features: <br><ul><li><code>II_FETCH_ROOT_KEY</code></li><li><code>II_DUMMY_CAPTCHA</code></li></ul>| [💾](https://github.com/dfinity/internet-identity/releases/latest/download/internet_identity_test.wasm) |
| Development | This flavor contains a version of Internet Identity that effectively performs no checks. It can be useful for external developers who want to integrate Internet Identity in their project and care about the general Internet Identity authentication flow, without wanting to deal with authentication and, in particular, WebAuthentication. Includes the following features: <br><ul><li><code>II_FETCH_ROOT_KEY</code></li><li><code>II_DUMMY_CAPTCHA</code></li><li><code>II_DUMMY_AUTH</code></li><li><code>II_INSECURE_REQUESTS</code></li></ul><br>See the [`using-dev-build`](demos/using-dev-build/README.md) project for an example on how to use this flavor.| [💾](https://github.com/dfinity/internet-identity/releases/latest/download/internet_identity_dev.wasm) |

## Stable Memory Compatibility

Internet Identity requires data in stable memory to have a specific layout in order to be upgradeable. The layout has been changed multiple times in the past. This is why II stable memory is versioned and each version of II is only compatible to some stable memory versions.

If on upgrade II traps with the message `stable memory layout version ... is no longer supported` then the stable memory layout has changed and is no longer compatible.

The easiest way to address this is to reinstall the canister (thus wiping stable memory). A canister can be reinstalled by executing `dfx deploy <canister> --mode reinstall`.

## Getting Help

We're here to help! Here are some ways you can reach out for help if you get stuck:

* [Internet Identity Bug Tracker](https://github.com/dfinity/internet-identity/issues): Create a new ticket if you encounter a bug using Internet Identity, or if an issue arises when you try to build the code.
* [DFINITY Forum](https://forum.dfinity.org/c/internet-identity/32): The forum is a great place to look for information and to ask for help.
* [Support](https://support.dfinity.org/hc/en-us/requests/new): Create a support request if you'd like to keep things private.

## Links

* [Internet Identity Specification][spec], the official Internet Identity Specification
* [Integration with Internet Identity](http://kyle-peacock.com/blog/dfinity/integrating-internet-identity/) by Kyle Peacock
* [What is Internet Identity?](https://internetcomputer.org/docs/current/tokenomics/identity-auth/what-is-ic-identity) on [internetcomputer.org](https://internetcomputer.org)
* [Internet Identity presentation 📼](https://youtu.be/oxEr8UzGeBo) on YouTube, streamed during the Genesis Event
* [Excalidraw](https://excalidraw.com), used to make diagrams

[Distrikt]: https://distrikt.io
[WebAuthn]: https://webauthn.guide
[DSCVR]: https://dscvr.one

[HACKING]: ./HACKING.md#running-locally
[ic]: https://internetcomputer.org
[spec]: https://internetcomputer.org/docs/current/references/ii-spec
[releases]: https://github.com/dfinity/internet-identity/releases
[Docker]: https://docker.io
[links]: #links
[candid]: https://internetcomputer.org/docs/current/developer-docs/build/languages/candid/candid-concepts/
