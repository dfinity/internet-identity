{ system ? builtins.currentSystem }:
(import ../default.nix {inherit system;}).ic-ref
