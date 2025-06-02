---
description: Perform some checks and fix them before pushing the branch
---

1. Run `cargo fmt`.
2. If any changes were made in step 1, commit them with message "Fix fomat".
3. Run `cargo clippy`.
4. If it fails, check the output of `cargo clippy`, fix it and run `cargo clippy` again. Repeat for a maximum of three times or until `cargo clippy` is successful.
5. If any changes were made in step 4, commit them with message "fix cargo clippy"
