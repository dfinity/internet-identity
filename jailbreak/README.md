# Jailbroken Internet Identity

This contains a patch that completely jailbreaks Internet Identity. This means
that all identities created use the exact same private key.

⚠️ This patch should never, ever land in the production code! ⚠️

This is useful when testing integration with II. Using this build, developers
do not have to care at all about setting up key pairs or using
WebAuthentication.

NOTE: should we blacklist the public key used in the jailbreak on prod?

## How to use

```bash
$ II_ENV=development USE_DUMMY_CAPTCHA=1 ./scripts/docker-build
$ dfx start
$ dfx canister --no-wallet install --mode reinstall internet_identity --argument '(null)'
```

now in `jailbreak/`:

```bash
$ npm ci
$ npm run test
```
