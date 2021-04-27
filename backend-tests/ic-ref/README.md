Internet Computer Reference
===========================

This repository contains

 * the source files of the [Interface Spec], which describes the externally
   visible behaviour of the Internet Computer.
   See [`README`](./spec/README.md) in `spec/`.

   [Interface Spec]: https://docs.dfinity.systems/public/v/

 * a reference implementation of this spec.
   Please see [`README`](./impl/README.md) in `impl/` for more on the goals,
   scope and design of this implementation.

 * an _acceptance test suite_, i.e. a program that will interact with any
   implementation of the Internet Computer (`ic-ref` or the “real” one) and
   check adherence to the specification. Naturally, this will always ever be
   partial; the fact that the test suite passes does _not_ guarantee that the
   implementation is in compliance.

   The code for this lives (for now) also in `impl/`.

   This is useful to aid in the development of the reference implementation,
   and would be even more useful the more implementations of the spec exists
   (e.g. due to multi-versioning).

About the Interface Spec
---------------------

This document describes the external interface of the Internet Computer. It is the authoritative source for interface details (request and function names, parameters, encodings). The goal is to have a document that is authoritative, and provides a place and a language to discuss external features of the Internet Computer in a hopefully concrete way. It could also be a document that we can publish to users of the Internet Computer.

Because of its focus on the externally visible behavior of the Internet Computer, it will also help uncover abstraction leaks. Because it aims to describe the full behavior, it helps to show which designs are unexpectedly complicated or don’t go well together. But as it intentionally does not address _how_ to implement this behaviour, it cannot be used as an implementation spec.

=== Status

The `master` version of this document, which claims version number `∞`, corresponds to a collection of finished and approved designs, but typically reflect neither the current status nor target of implementation.

The released versions, which can be found at <https://docs.dfinity.systems/public/v/>, are targetted by the production implementations (e.g. `replica`, `dfx`). Implementations do not necessarily target the latest released version; chosing an implementation target is a deliberate step coordinated between the involved parties. See below for  more information on versioning and release process.

Please skim the [list of open PRs](https://github.com/dfinity-lab/ic-ref/pulls?q=is%3Apr+is%3Aopen+%22Spec%22+in%3Atitle) against this document, to get an overview of ongoing discussions.

=== Process

This document is maintained by its editors (currently Joachim, Jens, Björn). The editors drive its evolution, make sure the right process is followed and try to keep tabs on relevant design discussions happening. Nevertheless, everyone is invited to contribute.

A proposed change of this document should take the form of a Pull Request on GitHub, be titled “Spec: _title_”, and its description should answer the following questions:

 1. Which parties (Replica, Languages, SDK, Research) are affected by this change, and why?
 2. Is this a a breaking change or not (important for versioning)
 3. If design slides are required: Which design slides is this based on, and – if not immediate – why do these slides imply the present change. This may initially refer to not-yet-approved slides, if the slides and the interface of a feature are developed side-by-side.

Changes will be merged into `master` when the design is finished. The editors will apply common sense to determine if all stakeholders have been heard and the design is complete.

Generally, PRs should be merged using “squash merge”.

Draft PRs can be used for experimentation and exploration without any process requirement.


Versioning
----------

The Interface Spec is versioned, using a three-component version like

    0.2.1

Releases from this repository are tagged using a three-component _code
version_ number:

    0.8.1
    ┬ ┬ ┬
    │ │ └ The third component is bumped upon non-breaking changes to the spec.
    │ └ The second component is bumped with a breaking change to the spec
    └ Always zero for now.

In tagged revisions, reference implementation should fully implement the spec.

In-between tagged revisions, this may not be the case, as the reference implementation catches up; in this case the reference implementation reports version `0.8.1-wip`

Each major spec version has a release branch (e.g. `release-0.8`) that only sees
non-breaking changes and bugfixes. A release branch should typically be “ahead” of all previous release branches.

The `master` branch contains finished designs, but is not directly scheduled
for implementation. It lists version version number `∞`. The reference
implementation on this branch typically does _not_ fully implement the spec. This branch should always be “ahead” of all the release branches.

Release process
---------------

Steps towards release `0.x.z`:

1. If this is a breaking change (and `z=0`):\
   Then create a new branch `release-0.x` off `release-0.(x-1)`
   Else continue on the existing `release-0.x`
2. In `spec/index.adoc`, change the version header to `0.x.z-wip`
3. In `impl/src/IC/Version.hs` set `version` to `0.x.z-wip`
4. In `spec/changelog.adoc`, begin a section for `0.x.z`
5. Cherry-pick desired changes from `master` or elsewhere.
6. Implement these changes in `ic-ref`/`ic-ref-test`.
7. Check `spec/changelog.adoc`
8. In `spec/index.adoc` and `impl/src/IC/Version.hs`, remove the `-wip` suffix.
   In `spec/changelog.adoc`, add the date to changelog entry.\
   Commit this. This is the release version.\
   `git tag 0.x.z`
9. Merge `release-0.x` back into `master`, making sure that the warning in
   section “Status” is still there, and that no changes are lost that did not
   make it into the release.

If you do non-spec-changes (typos in the text, changes to the implementation),
you can do them on a release branch after performing step 2 above (adding a changelog entry with the following version and `-wip` appended).

Exported build artifacts
------------------------

Other projects may depend on this repository, e.g. to use `ic-ref` in the test
suite. For their benefit, this repo provides the following public interface:

The `ic-ref` repository defines at least the following nix derivations, as
attributes of the top-level `default.nix`:

* `ic-ref`: contains `bin/ic-ref`, `bin/ic-ref-run`, `bin/ic-request-id`, `bin/ic-ref-test`
* `interface-spec`: contains `spec/index.html`

Reports
-------

Note that these links point to the `master` versions of the report, not necessarily
the release or branch you are looking at yourself

* [Test suite output](https://hydra.dfinity.systems/latest/dfinity-ci-build/ic-ref/ic-ref-test/) of running `ic-ref-test` against `ic-ref`.
* [Coverage report](https://hydra.dfinity.systems/latest/dfinity-ci-build/ic-ref/coverage/) of the same. This can be used to find untested behaviour.
