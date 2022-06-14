/**
 * Here you'll find the various modules related to testing the II canister:
 *  * `tests`: The most important module with the actual tests
 *  * `api`: Rust-bindings for the II canister
 *  * `framework`: Helpers of various kinds for writing tests.
 *  * `flows`: Reusable flows consisting of multiple II interactions
 *
 * Most changes should happen in the `tests` module. The split was done this way so that `tests` is a simple
 * as possible to make tests easy to read and write.
 *
 * Modules are not imported for non-test builds. This crate only contains tests and should not
 * require anything to be built otherwise.
 */

#[cfg(test)]
mod tests;

#[cfg(test)]
mod api;

#[cfg(test)]
mod framework;

#[cfg(test)]
mod flows;
