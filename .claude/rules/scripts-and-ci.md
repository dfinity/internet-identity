---
paths:
  - "scripts/**"
  - ".github/**"
  - "Dockerfile"
  - "package.json"
  - "Cargo.toml"
  - "**/Cargo.toml"
---

# Scripts, CI and dependencies

## Shell scripts

- Fail loudly: `set -euo pipefail`, quote every expansion, propagate failures (`|| return 1`), validate inputs up front. Never swallow errors with `2>/dev/null` or `|| true`.
- Run on macOS (bash 3.2, BSD tools: no `nproc`, no GNU-only flags) and on Linux CI (no `sed -i ''`, no `tee /dev/tty`). Tell an empty variable apart from an unset one (`${VAR+set}`) under `set -u`.
- Don't mutate the working tree; use `mktemp -d`. Write multi-file updates atomically.
- Hoist hardcoded values (canister ids, ports, URLs) into named variables at the top.
- Paginate `gh api` calls. Derive release and proposal facts from on-chain state (the running wasm hash), not from GitHub releases.
- Pin and checksum-verify every tool binary a script downloads.

## Workflows

- Every workflow has an explicit minimal `permissions:` block (`contents: read` by default).
- Secret-bearing workflows reject forks or limit `author_association`, pass event data through `env:`, and use a PAT only on `push`. Auto-commit steps run only on `push`.
- Pin tool versions to the repo's declared ones (`.icp-cli-version`, `.node-version`, the `POCKET_IC_SERVER_VERSION` pin); never install `latest`. Keep `.node-version` and `package.json` `engines` in sync.
- A gate that fails to look something up fails the job; it never skips tests.
- Diagnose a CI failure from its logs before rerunning, and say on the PR when it was a flake and why.

## Dependencies

- Dependency updates come from Dependabot or the team's workspace-wide update (`scripts/update-deps`). Lockfile-only and transitive Dependabot bumps are closed and picked up by the next workspace update. Don't add dependency bumps to feature PRs.
- A bot dependency PR that needs code changes is closed and redone as a manual PR.
- Commit `Cargo.lock` in sync with `Cargo.toml` (CI checks `--locked`).
- List every added, removed or changed dependency in the PR description with a clear reason: what it is for, why this one, its size and maintenance, and what it replaces. Never put the reason in a comment in `Cargo.toml` or `package.json`.

## Feature flags and releases

- Ship in-progress features behind a flag (set through canister init args) so a release can be cut at any time. Never commit a flag default flipped to `true` for local testing.
- Leave links to historical release assets (and `releases/latest`) alone until a release has published the renamed asset.
