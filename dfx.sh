#!/usr/bin/env bash

nix run '((import (builtins.fetchGit { url = "git@github.com:dfinity-lab/sdk"; ref = "joachim/idp";}) {}).dfx.standalone)' -c dfx "$@"
