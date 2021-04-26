{ system ? builtins.currentSystem }:
let
  sourcesnix = builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.19/nix/sources.nix;
    sha256 = "1n92ka2rkdiib6ian6jh2b7fwvklnnwlp5yy5bv6ywm7m1y5hyfl";
  };
  nixpkgs_src = (import sourcesnix { sourcesFile = ./sources.json; inherit pkgs; }).nixpkgs;

  bootstrap-pkgs = import nixpkgs_src {
    system = builtins.currentSystem;
  };

  nixpkgs-patched = bootstrap-pkgs.applyPatches {
    name = "nixpkgs-patched";
    src = nixpkgs_src;
    patches = [
      ./patches/0001-ghc865-binary-Use-binary-distribution-which-links-ag.patch
      ./patches/0002-openblas-0.3.10-0.3.13.patch
      ./patches/fb063991b26b2b93dece6d09f37041451a5ef4cb.patch
    ];
  };

  pkgs =
    import nixpkgs-patched {
      inherit system;
      overlays = [
        (self: super: {
          sources = import sourcesnix { sourcesFile = ./sources.json; pkgs = super; };


          # nixpkgs's rustc does not inclue the wasm32-unknown-unknown target, so
          # lets add it here. With this we can build the universal canister with stock
          # nixpkgs + naersk, in particular no dependency on internal repositories.
          rustc = super.rustc.overrideAttrs (old: {
            configureFlags = self.lib.lists.forEach old.configureFlags (flag:
              if self.lib.strings.hasPrefix "--target=" flag
              then flag + ",wasm32-unknown-unknown"
              else flag
            );
          });

          all-cabal-hashes = self.fetchurl {
            url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/f18d8ab7adfbd15acfc5e994dfb973577a5aba5c.tar.gz";
            sha256 = "0kn2wqilpw0nyx54jyz4vp7xrx1893zdv7d54yi9pjl677pnwcs9";
          };
        })
      ];
    };
in
pkgs
