{ system ? builtins.currentSystem }:
(import ../default.nix {inherit system;}).interface-spec
