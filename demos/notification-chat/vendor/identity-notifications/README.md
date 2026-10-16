# Vendored identity-notifications

A copy of the `rust/` crate from
[dfinity/identity-notifications](https://github.com/dfinity/identity-notifications)
at `0aa141d`, source only: no tests, no examples, no workspace.

It is here because that repository is internal and this one is public, so CI
cannot fetch it as a git dependency. Replace this directory with a normal
dependency once the library is public or published:

```toml
identity-notifications = "0.1"
```

Refresh it by copying `rust/src` and `rust/Cargo.toml` over again, keeping the
manifest trimmed the same way.
