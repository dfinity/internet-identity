# Code style (all languages)

## Names

- Name things for what they are and do. Values and types are nouns, functions are verbs, predicates are `is_*`/`has_*`/`should_*` (`isOpen`, `isLoading` in TS). Never use an adjective for a binding (`cleared`, `available`): say what it holds (`num_available_slots`).
- A function named like a lookup must not write. Use `ensure_*` for get-or-create, `check_*` for a pure check, `validate_*` for a check that returns the reason it failed.
- Prefer long, unabbreviated names over short ones, locals included (`existing_devices`, not `devs`; no `cb`, `cfg`). Reuse the terms the surrounding code and UI already use, and don't coin new ones unless the design introduces the concept.
- Put the unit in the name of every quantity: `_ns`, `_SECS`, `_MILLIS`, `_BYTES` (`expires_at_ns`, `MAX_SESSION_KEY_BYTES`). Don't rename a shipped candid field just to add one.
- No magic numbers. Lift a value to a named constant at the top of the file, give it a doc comment saying why it has that value, and link the source of truth when it is defined elsewhere. Give each concern its own constant even when two values match today.
- Use US spelling in identifiers and comments (`normalized`, `defense`).

## Comments and doc comments

Reviewers remove more comments than they ask for. Let names carry the meaning, and write a comment only for what the code cannot say.

- Comment the *why* that isn't visible: the constraint, the threat guarded against, the bug avoided, where a value comes from. Never narrate what the next line does.
- Every constant and every new type gets a doc comment. A module that implements a non-trivial mechanism opens with a doc comment stating the problem it solves and how.
- Doc comments must make sense on their own: name every value they mention and define any term. Don't explain a function by pointing at its call site.
- Comments describe the current code only. No history ("previously", "now uses"), no PR or issue numbers, no reasons for what is absent, no design-process justification ("we chose this because the reviewer..."). History belongs in git and the PR.
- Don't copy the same comment to every place a property appears. Say it once where it is defined.
- When you change behaviour, re-read every doc comment, `.did` comment and module header that describes it, and fix units and counts. Never cite a test, module, spec or function that doesn't exist.

## Scope and structure

- Don't add props, options, fields, exports, versions or concepts "for later". Add them with their first caller.
- Before writing a helper, search for one: local utils, then `$lib/utils/` or the crate's `utils.rs`, then a well-maintained dependency already in the tree, then the platform. Prefer a maintained library over hand-rolled parsers, codecs and crypto.
- Accept a little duplication across features over a premature shared abstraction; extract on the third use.
- Prefer early-return guard clauses over nested `if`/`else`: preconditions first, then the happy path.
- Use explicit comparisons (`!== undefined`, `=== null`, `.length > 0`) for non-boolean values.
- Leave legacy and deprecated code paths alone unless the task is to change them. Evolve the replacement.
- Remove debug leftovers before pushing: `console.log`, `println!`, `.only`, commented-out code, hard-coded returns, feature-flag defaults flipped to `true` for local testing.
