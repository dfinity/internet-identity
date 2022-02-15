# Internet Identity with dummy auth

These are the tests run against the `II_DUMMY_AUTH=1` build of Internet
Identity.

⚠️ This build should never, ever land in production! ⚠️

This is useful when testing integration with II. Using this build, developers
do not have to care at all about setting up key pairs or using
WebAuthentication.

TODO:
* should we blacklist the public key used in the jailbreak on prod?
* should we show a banner on the landing page?
* should we show an error on ic0.app?

## How to use

In the top-level directory:

```bash
$ II_ENV=development II_DUMMY_CAPTCHA=1 II_DUMMY_AUTH=1 ./scripts/docker-build
$ dfx start
$ dfx canister --no-wallet install --mode reinstall internet_identity --argument '(null)'
```

now in `dummy-auth-tests/`:

```bash
$ npm ci
$ npm run test
```
