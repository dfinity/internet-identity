{
  system ? builtins.currentSystem,
}:

let nixpkgs = import ./nix { inherit system; }; in
let stdenv = nixpkgs.stdenv; in
let subpath = p: import ./nix/gitSource.nix p; in

let naersk = nixpkgs.callPackage nixpkgs.sources.naersk {}; in
let universal-canister = (naersk.buildPackage rec {
    name = "universal-canister";
    src = subpath ./universal-canister;
    root = ./universal-canister;
    CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "${nixpkgs.llvmPackages_9.lld}/bin/lld";
    RUSTFLAGS = "-C link-arg=-s"; # much smaller wasm
    cargoBuildOptions = x : x ++ [ "--target wasm32-unknown-unknown" ];
    doCheck = false;
    release = true;
}).overrideAttrs (old: {
    postFixup = (old.postFixup or "") + ''
      mv $out/bin/universal_canister $out/universal_canister.wasm
      rmdir $out/bin
    '';
}); in


let haskellPackages = nixpkgs.haskellPackages.override {
  overrides = import nix/haskell-packages.nix nixpkgs subpath;
}; in

let
  ic-ref = nixpkgs.haskell.lib.dontCheck (
    haskellPackages.ic-ref.overrideAttrs (old: {
      installPhase = (old.installPhase or "") + ''
        cp -rv test-data $out/test-data
        # replace symlink with actually built
        rm -f $out/test-data/universal_canister.wasm
        cp ${universal-canister}/universal_canister.wasm $out/test-data
      '';
      # variant of justStaticExecutables that retains propagatedBuildInputs
      postFixup = "rm -rf $out/lib $out/share/doc";
    })
  );

  # This is a static build of the ic-ref tool only,
  # for distribution independent of nix
  ic-ref-dist =
    if nixpkgs.stdenv.isDarwin
    # on Darwin, use dylibbundler to include non-system libraries
    then nixpkgs.runCommandNoCC "ic-ref-dist" {
        buildInputs = [ nixpkgs.macdylibbundler nixpkgs.removeReferencesTo ];
        allowedRequisites = [];
      } ''
        mkdir -p $out/bin
        cp ${ic-ref}/bin/ic-ref $out/bin
        chmod u+w $out/bin/ic-ref
        dylibbundler \
          -b \
          -x $out/bin/ic-ref \
          -d $out/bin \
          -p '@executable_path' \
          -i /usr/lib/system \
          -i ${nixpkgs.darwin.Libsystem}/lib

        # there are still plenty of nix store references
        # but they should not matter
        remove-references-to \
          -t ${nixpkgs.darwin.Libsystem} \
          -t ${nixpkgs.darwin.CF} \
          -t ${nixpkgs.libiconv} \
          $out/bin/*

        # sanity check
        $out/bin/ic-ref --version
      ''

    # on Linux, build statically using musl
    # and until we are open source, also using integer-simple
    # (once we can use ghc-9.0 we can maybe use ghc-bignum native, which should be faster)
    else
      let
        muslHaskellPackages = nixpkgs.pkgsMusl.haskell.packages.integer-simple.ghc884.override {
          overrides = self: super:
            import nix/haskell-packages.nix nixpkgs subpath self super
            // {
              cryptonite = super.cryptonite.overrideAttrs(old: {
                configureFlags = "-f-integer-gmp";
                doCheck = false; # test suite too slow without integer-gmp
              });
              # more test suites too slow withour integer-gmp
              scientific = nixpkgs.haskell.lib.dontCheck super.scientific;
              math-functions = nixpkgs.haskell.lib.dontCheck super.math-functions;
            };
        };
        ic-ref-musl =
          muslHaskellPackages.ic-ref.overrideAttrs (
            old: {
              configureFlags = [
                "--ghc-option=-optl=-static"
                "--extra-lib-dirs=${nixpkgs.pkgsMusl.zlib.static}/lib"
                "--extra-lib-dirs=${nixpkgs.pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
              ];
            }
          );
        in nixpkgs.runCommandNoCC "ic-ref-dist" {
          allowedRequisites = [];
        } ''
          mkdir -p $out/bin
          cp ${ic-ref-musl}/bin/ic-ref $out/bin
        '';


  # We run the unit test suite only as part of coverage checking (saves time)
  ic-ref-coverage = nixpkgs.haskell.lib.doCheck (nixpkgs.haskell.lib.doCoverage ic-ref);
in



rec {
  inherit ic-ref;
  inherit ic-ref-dist;
  inherit ic-ref-coverage;
  inherit universal-canister;

  ic-ref-test = nixpkgs.runCommandNoCC "ic-ref-test" {
      nativeBuildInputs = [ ic-ref ];
    } ''
      function kill_ic_ref () { kill %1; }
      ic-ref --pick-port --write-port-to port &
      trap kill_ic_ref EXIT PIPE
      sleep 1
      test -e port
      mkdir -p $out
      LANG=C.UTF8 ic-ref-test --endpoint "http://0.0.0.0:$(cat port)/" --html $out/report.html

      mkdir -p $out/nix-support
      echo "report test-results $out report.html" >> $out/nix-support/hydra-build-products
    '';

  coverage = nixpkgs.runCommandNoCC "ic-ref-test" {
      nativeBuildInputs = [ haskellPackages.ghc ic-ref-coverage ];
    } ''
      function kill_ic_ref () { kill  %1; }
      ic-ref --pick-port --write-port-to port &
      trap kill_ic_ref EXIT PIPE
      sleep 1
      test -e port
      LANG=C.UTF8 ic-ref-test --endpoint "http://0.0.0.0:$(cat port)/"
      kill -INT %1
      trap - EXIT PIPE
      sleep 5 # wait for ic-ref.tix to be written

      find
      LANG=C.UTF8 hpc markup ic-ref.tix --hpcdir=${ic-ref-coverage}/share/hpc/vanilla/mix/ic-ref --srcdir=${subpath ./impl}  --destdir $out

      mkdir -p $out/nix-support
      echo "report coverage $out hpc_index.html" >> $out/nix-support/hydra-build-products
    '';

  # The following two derivations keep the impl/cabal.products.freeze files
  # up to date. It is quite hacky to get the package data base for the ic-ref
  # derivation, and then convince Cabal to use that...
  cabal-freeze = (nixpkgs.haskell.lib.doCheck haskellPackages.ic-ref).overrideAttrs(old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ nixpkgs.cabal-install ];
      phases = [ "unpackPhase" "setupCompilerEnvironmentPhase" "buildPhase" "installPhase" ];
      buildPhase = ''
        rm -f cabal.project.freeze cabal.project
        unset GHC_PACKAGE_PATH
        mkdir .cabal
        touch .cabal/config # empty, no repository
        HOME=$PWD cabal v2-freeze --ghc-pkg-options="-f $packageConfDir" --offline --enable-tests || true
      '';
      outputs = ["out"]; # no docs
      installPhase = ''
        mkdir -p $out
        echo "-- Run nix-shell .. -A check-cabal-freeze to update this file" > $out/cabal.project.freeze
        cat cabal.project.freeze >> $out/cabal.project.freeze
      '';
    });

  check-cabal-freeze = nixpkgs.runCommandNoCC "check-cabal-freeze" {
      nativeBuildInputs = [ nixpkgs.diffutils ];
      expected = cabal-freeze + /cabal.project.freeze;
      actual = ./impl/cabal.project.freeze;
      cmd = "nix-shell . -A check-cabal-freeze";
      shellHook = ''
        dest=${toString ./impl/cabal.project.freeze}
        rm -f $dest
        cp -v $expected $dest
        chmod u-w $dest
        exit 0
      '';
    } ''
      diff -r -U 3 $actual $expected ||
        { echo "To update, please run"; echo "nix-shell . -A check-cabal-freeze"; exit 1; }
      touch $out
    '';

  check-generated = nixpkgs.runCommandNoCC "check-generated" {
      nativeBuildInputs = [ nixpkgs.diffutils ];
      expected = import ./nix/generate.nix { pkgs = nixpkgs; };
      dir = ./nix/generated;
    } ''
      diff -r -U 3 $expected $dir
      touch $out
    '';

  # A simple license check: Check that all used Haskell packages
  # declare a liberal (non-GPL) license.
  # This does not necessarily cover imported C libraries!
  license-check = haskellPackages.ic-ref.overrideAttrs(old: {
      name = "ic-ref-license-check";
      phases = [ "unpackPhase" "setupCompilerEnvironmentPhase" "buildPhase" "installPhase" ];
      buildPhase = ''
        cd $packageConfDir
        ! grep -i '^license:' *.conf | grep -v 'BSD\|Apache\|MIT\|ISC'
      '';
      outputs = ["out"]; # no docs
      installPhase = ''
        touch $out
      '';
    });


  interface-spec =
    nixpkgs.stdenv.mkDerivation {
    name = "interface-spec";
    src = subpath ./spec;
    phases = [ "unpackPhase" "buildPhase" "checkPhase" ];
    buildInputs = with nixpkgs;
      [ asciidoctor plantuml jre graphviz python cpio html-proofer cddl ];
    FONTCONFIG_FILE = nixpkgs.makeFontsConf { fontDirectories = []; };
    asciidoctor_args = [
      "-r asciidoctor-diagram"
      "-a sectanchors"
      "-a plantuml-format=svg"
      "-a ditaa-format=svg"
      "-a graphviz-format=svg"
      "-a source-highlighter=rouge"
      "-a rouge-css=style"
    ];
    buildPhase = ''
      doc_path="spec"
      mkdir -p $out/$doc_path
      asciidoctor $asciidoctor_args --failure-level WARN -v \
        -a toc2 -a toclevels=3 \
        -a example -a partial \
        -a attachmentsdir=. \
        -R $PWD -D $out/$doc_path/ index.adoc
      find . -type f -name '*.png' | cpio -pdm $out/$doc_path/
      cp *.cddl $out/$doc_path
      cp ic.did $out/$doc_path


      mkdir -p $out/nix-support
      echo "report spec $out/$doc_path index.html" >> $out/nix-support/hydra-build-products
    '';

    # These ones are needed for htmlproofer
    LOCALE_ARCHIVE = nixpkgs.lib.optionalString nixpkgs.stdenv.isLinux "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    LC_TYPE = "en_US.UTF-8";
    LANGUAGE = "en_US.UTF-8";
    doCheck = true;
    checkPhase = ''
      htmlproofer --disable-external $out/$doc_path
      if [[ ! -s $out/$doc_path/index.html ]]; then
        >&2 echo "There is no $out/$doc_path/index.html or it is empty, aborting."
        exit 1
      fi

      # also check cddl
      for file in *.cddl; do cddl $file generate 1 > /dev/null; done
    '';

  };

  # for compatibility with the deployment to https://docs.dfinity.systems/public/v
  public-spec = interface-spec;

  all-systems-go = nixpkgs.releaseTools.aggregate {
    name = "all-systems-go";
    constituents = [
      ic-ref
      ic-ref-dist
      ic-ref-test
      universal-canister
      interface-spec
      check-generated
    ];
  };

  # include shell in default.nix so that the nix cache will have pre-built versions
  # of all the dependencies that are only depended on by nix-shell.
  ic-ref-shell =
    let extra-pkgs = [
      nixpkgs.cabal-install
      nixpkgs.ghcid
    ]; in

    haskellPackages.ic-ref.env.overrideAttrs (old: {
      propagatedBuildInputs = (old.propagatedBuildInputs or []) ++ extra-pkgs ;
    });
}
