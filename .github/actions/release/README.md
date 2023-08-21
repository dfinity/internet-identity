# Release notes action

This action creates nice release notes.

## Usage

To try the action locally, run the following:

``` bash
$ # The list of files for which we compute the sha256
$ # (those file must exist, though they don't need to have meaningful content)
$ export INPUT_ASSETS='internet_identity_production.wasm.gz
internet_identity_dev.wasm.gz
internet_identity_test.wasm.gz
archive.wasm.gz'
$ export INPUT_PRODUCTION_ASSET=internet_identity_production.wasm.gz
$ export RELEASE_TAG=release-2023-08-11 # Does not need to exist
# If you want to test the CI links to the sha256sum steps, also provide a valid INPUT_WORKFLOW_JOBS
# The easiest way to get example data to use for INPUT_WORKFLOW_JOBS is to grab it from the last release CI run
# E.g. here: https://github.com/dfinity/internet-identity/actions/runs/5900896849/job/16006090306#step:10:24
$ export INPUT_WORKFLOW_JOBS=$(cat workflow_jobs.json)
# If you want a meaningful changelog, also provide a valid $INPUT_CHANGELOG
# The easiest way to get example data to use for INPUT_CHANGELOG is to grab it from the last release CI run
# E.g. here: https://github.com/dfinity/internet-identity/actions/runs/5900896849/job/16006090306#step:10:27
$ export INPUT_CHANGELOG=$(cat changelog.md)
$ ./.github/actions/release/run.sh # finally, run the action
```
