<p align="center"><a href="https://identity.internetcomputer.org" target="_blank" rel="noopener noreferrer"><img width="600" src="./ii-logo.png" alt="Internet Identity"/></a></p>

<p align="center">
    <a href="https://github.com/dfinity/internet-identity/actions/workflows/canister-tests.yml"><img src="https://github.com/dfinity/internet-identity/actions/workflows/canister-tests.yml/badge.svg" alt="Canister Tests" /></a>
    <a href="https://github.com/dfinity/internet-identity/actions/workflows/rust.yml"><img src="https://github.com/dfinity/internet-identity/actions/workflows/rust.yml/badge.svg" alt="Rust" /></a>
    <a href="https://github.com/dfinity/internet-identity/actions/workflows/frontend-checks.yml"><img src="https://github.com/dfinity/internet-identity/actions/workflows/frontend-checks.yml/badge.svg" alt="Frontend checks and lints" /></a>
    <a href="https://github.com/dfinity/internet-identity/releases"><img src="https://img.shields.io/github/downloads/dfinity/internet-identity/total?label=downloads&logo=github" alt="GitHub all releases" /></a>
</p>

<p align="center">
    🔗 <a href="https://identity.internetcomputer.org">https://identity.internetcomputer.org</a> • 📜 <a href="https://internetcomputer.org/docs/current/references/ii-spec">Specification</a> <br/> ― <br/>📚 <a href="https://forum.dfinity.org/c/internet-identity/32">Forum</a> • 🚑 <a href="https://github.com/dfinity/internet-identity/issues/new">Report an Issue</a> • 📞 <a href="https://discord.internetcomputer.org">Discord</a>
</p>

---

Internet Identity is an authentication and identity service for the [Internet Computer][ic]. It enables hundreds of thousands of users to securely log in to dapps such as [OISY](https://oisy.com), [Caffeine](https://caffeine.ai), [NNS Dapp](https://nns.internetcomputer.org), [OpenChat](https://oc.app), and [many more](https://identity.internetcomputer.org).

Internet Identity is:

- **Simple**: Uses [passkeys][webauthn] (TouchID, FaceID, Windows Hello, security keys) for passwordless registration and authentication. Users can also sign in with their Google, Apple, or Microsoft account via [OpenID Connect](https://openid.net/developers/how-connect-works/).
- **Flexible**: Integrating Internet Identity is as simple as using the [agent-js](https://github.com/dfinity/agent-js) library or the ICRC authentication standards. No need to interact with the canister smart contract directly.
- **Secure**: A unique identity (principal) is derived for each app a user authenticates to, preventing cross-app tracking. Cryptographic material never leaves the user's device.

For more information, see the [Internet Identity documentation](https://internetcomputer.org/docs/current/developer-docs/integrations/internet-identity/) on [internetcomputer.org](https://internetcomputer.org).

### Table of Contents

- [Getting Started](#getting-started)
  - [Local Replica](#local-replica)
  - [Architecture Overview](#architecture-overview)
  - [Building with Docker](#building-with-docker)
  - [Integration with Internet Identity](#integration-with-internet-identity)
- [Key Features](#key-features)
- [Technology Stack](#technology-stack)
- [Stable Memory Compatibility](#stable-memory-compatibility)
- [Contributing](#contributing)
- [Getting Help](#getting-help)
- [Links](#links)

## Getting Started

This section gives an overview of Internet Identity's architecture, instructions on how to build the Wasm module (canister), and finally pointers for integrating Internet Identity in your own applications.

### Local Replica

Use the Internet Identity canister in your local icp-cli project by adding the following snippet to the `canisters` array in your `icp.yaml` file:

```yaml
canisters:
  - name: internet_identity
    build:
      steps:
        - type: pre-built
          url: https://github.com/dfinity/internet-identity/releases/download/release-2025-04-04-v3/internet_identity_production.wasm.gz
    init_args: "(opt record { captcha_config = opt record { max_unsolved_captchas= 50:nat64; captcha_trigger = variant {Static = variant {CaptchaDisabled}}}})"
```
```

To deploy, run `icp deploy`.

To access Internet Identity or configure it for your dapp, use one of the following URLs:

- Chrome, Firefox: `http://<canister_id>.localhost:4943`
- Safari: `http://localhost:4943?canisterId=<canister_id>`

#### Note on Apple Silicon

If you are running into build issues on Apple Silicon (specifically, if rollup is trying to access @rollup/rollup-darwin-x64), try using nvm to install node.

### Architecture Overview

Internet Identity is an authentication service for the [Internet Computer][ic]. All programs on the Internet Computer are Wasm modules, or canisters (canister smart contracts).

![Architecture](./ii-architecture.png) <!-- this is an excalidraw.com image, source is ii-architecture.excalidraw -->

Internet Identity consists of two canisters: a **backend canister** that manages user data and authentication logic, and a stateless **frontend canister** that serves the web application assets.

> 💡 The backend interface is specified by the [internet_identity.did](./src/internet_identity/internet_identity.did) [candid] interface. The backend canister code is located in [`src/internet_identity`](./src/internet_identity), and the frontend application code is located in [`src/frontend`](./src/frontend).

The Internet Identity authentication service works indirectly by issuing "delegations" on the user's behalf; basically attestations signed with some private cryptographic material owned by the user. The private cryptographic material never leaves the user's device. The Internet Identity frontend application uses the [WebAuthn] API to first create the private cryptographic material, and then the [WebAuthn] API is used again to sign delegations.

> Note: The architecture diagram above shows the core delegation flow. See the [specification][spec] for the full architecture.

For more details, please refer to the [Internet Identity Specification][spec].

### Building with Docker

To get the canister (Wasm module) for Internet Identity, you can either **download a release** from the [releases] page, or build the code yourself. The simplest way to build the code yourself is to use [Docker] and the [`docker-build`](./scripts/docker-build) script:

```bash
$ ./scripts/docker-build
```

The [`Dockerfile`](./Dockerfile) specifies build instructions for Internet Identity. Building the `Dockerfile` will result in a scratch container that contains the Wasm module at `/internet_identity.wasm.gz`.

We recommend using the [`docker-build`](./scripts/docker-build) script. It extracts the Wasm module from the final scratch container.

> 💡 You can find instructions for building the code without Docker in the [HACKING] document.

### Integration with Internet Identity

The [`using-dev-build`](./demos/using-dev-build) demo shows a minimal example project that integrates Internet Identity using [agent-js](https://github.com/dfinity/agent-js). For the full integration protocol, refer to the [Client Authentication Protocol section](https://internetcomputer.org/docs/current/references/ii-spec#client-authentication-protocol) of the [Internet Identity Specification][spec].

## Key Features

- **Passkey Authentication**: Passwordless login using WebAuthn-compatible authenticators (TouchID, FaceID, Windows Hello, security keys).
- **OpenID Connect**: Users can register and authenticate using their Google, Apple, or Microsoft account as an alternative to passkeys.
- **Multiple Accounts**: Users can create and manage multiple identities (accounts) per application.
- **Discoverable Passkeys**: Support for resident/discoverable credentials that enable sign-in without entering an identity number.

## Technology Stack

- **Backend**: Rust canister running on the Internet Computer
- **Frontend**: [SvelteKit](https://kit.svelte.dev/) + TypeScript, built with Vite
- **Authentication**: WebAuthn / passkeys, OpenID Connect (Google, Apple, Microsoft)
- **Specification**: [Internet Identity Spec][spec]

## Stable Memory Compatibility

Internet Identity requires data in stable memory to have a specific layout in order to be upgradeable. The layout has been changed multiple times in the past. This is why II stable memory is versioned and each version of II is only compatible to some stable memory versions.

If on upgrade II traps with the message `stable memory layout version ... is no longer supported` then the stable memory layout has changed and is no longer compatible.

The easiest way to address this is to reinstall the canister (thus wiping stable memory). A canister can be reinstalled by executing `icp deploy <canister> --mode reinstall`.

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](./CONTRIBUTING.md) for guidelines on how to get started. For development setup and build instructions, refer to [HACKING.md](./HACKING.md).

## Getting Help

We're here to help! Here are some ways you can reach out for help if you get stuck:

- [Internet Identity Bug Tracker](https://github.com/dfinity/internet-identity/issues): Create a new ticket if you encounter a bug using Internet Identity, or if an issue arises when you try to build the code.
- [DFINITY Forum](https://forum.dfinity.org/c/internet-identity/32): The forum is a great place to look for information and to ask for help.
- [Support](https://support.dfinity.org/hc/en-us/requests/new): Create a support request if you'd like to keep things private.

## Links

- [Internet Identity Specification][spec], the official Internet Identity Specification
- [Internet Identity Documentation](https://internetcomputer.org/docs/current/developer-docs/integrations/internet-identity/) on [internetcomputer.org](https://internetcomputer.org)

[webauthn]: https://webauthn.guide
[hacking]: ./HACKING.md#running-locally
[ic]: https://internetcomputer.org
[spec]: https://internetcomputer.org/docs/current/references/ii-spec
[releases]: https://github.com/dfinity/internet-identity/releases
[docker]: https://docker.io
[links]: #links
[candid]: https://internetcomputer.org/docs/current/developer-docs/build/languages/candid/candid-concepts/
