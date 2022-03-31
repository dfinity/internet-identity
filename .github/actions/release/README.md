# Release notes action

This action creates nice release notes.

## Usage

To try the action locally, run the following:

``` bash
$ # first, you need a GitHub Access token: https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token
$ # Make sure the command is prefixed with a space so that bash does not store in in its 
$ # history file.
$  export GITHUB_TOKEN="..."
$ # The list of files for which we compute the sha256
$ # (those file must exist, though they don't need to have meaningful content)
$ export INPUT_ASSETS='internet_identity_production.wasm
internet_identity_dev.wasm
internet_identity_test.wasm'
$ export RELEASE_TAG=release-2022-03-30 # Any tag
$ export GITHUB_SHA=$(git rev-parse $RELEASE_TAG)
$ export RELEASE_TAG_PREVIOUS=release-2022-03-15 # Any tag older than RELEASE_TAG
$ ./.github/actions/release/run # finally, run the action
```
