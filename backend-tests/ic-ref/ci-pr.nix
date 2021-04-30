{ src ? { rev = null; }, ... }:
let
  nixpkgs = import ./nix { };
  baseJobs = import src.mergeBase { system = "x86_64-linux"; };
  prJobs = import ./default.nix { system = "x86_64-linux"; };

  # This compares the sources of the Interface Spec of this PR with the base
  # commit, and if they differe, post a helpful comment on github.
  # Changes in changelog.adoc are ignored (they might refer to code changes)
  spec-changes =
    nixpkgs.runCommandNoCC "spec-changes" {
      nativeBuildInputs = [ nixpkgs.coreutils ];
    } ''
      mkdir -p $out

      if ! diff -q --recursive --exclude=changelog.adoc ${baseJobs.interface-spec.src} ${prJobs.interface-spec.src}
      then
        mkdir -p $out/nix-support
        comment_template=$out/nix-support/gh-comment
        cat <<EOF >$comment_template
      {{{! comment:edit-one }}}This PR changes the interface spec, you can preview the result at:
      {{{ host }}}/latest/{{{ project }}}/{{{ jobset }}}/interface-spec/1/index.html
      EOF
        echo "comment manifest $comment_template comment.mustache" >> $out/nix-support/hydra-build-products
      fi
    '';
in
import ./ci.nix { inherit src; } //
nixpkgs.lib.optionalAttrs (src ? mergeBase) {
  inherit spec-changes;
}
