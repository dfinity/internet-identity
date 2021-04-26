{ system ? builtins.currentSystem }:
(import ../default.nix {inherit system;}).universal-canister
