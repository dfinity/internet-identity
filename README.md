# idp_service

Welcome to your new idp_service project and to the internet computer development community. By default, creating a new project adds this README and some template files to your project directory. You can edit these template files to customize your project and to include your own code to speed up the development cycle.

To get started, you might want to explore the project directory structure and the default configuration file. Working with this project in your development environment will not affect any production deployment or identity tokens.

To learn more before you start working with idp_service, see the following documentation available online:

- [Quick Start](https://sdk.dfinity.org/docs/quickstart/quickstart-intro.html)
- [SDK Developer Tools](https://sdk.dfinity.org/docs/developers-guide/sdk-guide.html)
- [Motoko Programming Language Guide](https://sdk.dfinity.org/docs/language-guide/motoko.html)
- [Motoko Language Quick Reference](https://sdk.dfinity.org/docs/language-guide/language-manual.html)

To run the idp_service canister, proceed as follows

```bash
cd idp_service/src
dfx start --background
dfx canister create idp_service
dfx canister create frontend
dfx build
dfx canister install idp_service
```

Then the canister can be used as

```bash
dfx canister call idp_service register '(123, "test", {1; 2; 3}; null)'
```

