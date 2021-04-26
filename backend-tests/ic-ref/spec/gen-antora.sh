#!/usr/bin/env bash

#
# This script transforms the interface specification file
# to the right directory structure that is expected by antora,
# the tool that we use to fill the sdk website.
#
# A github action (see .github/workflows/antora.yml) pushes
# the result of this to branch public-antora
#

if [ -z "$1" ];
then
  echo "Usage $0 out/"
  exit 1
fi

in="$(realpath "$(dirname "${BASH_SOURCE[0]}")")"
out="$1"

if [ -e "$out" ];
then
  echo "Please delete $out first"
  exit 1
fi


mkdir -p "$out/modules/interface-spec"
mkdir -p "$out/modules/interface-spec/pages"
mkdir -p "$out/modules/interface-spec/partials"
mkdir -p "$out/modules/interface-spec/attachments"
mkdir -p "$out/modules/interface-spec/examples"

cat >> "$out/README.md" <<__END__
You are looking at the antora-public branch. This branch is
auto-created from the public branch via the spec/gen-antora.sh script.
Do not edit this branch, submit pull requests against it, or from it.
__END__

cat >> "$out/antora.yml" <<__END__
name: docs
version: master
__END__


cat >> "$out/modules/interface-spec/ic-nav.adoc" <<__END__
* xref:index.adoc[{IC} Interface Specification]
__END__


cp -v "$in/index.adoc" "$out/modules/interface-spec/pages/index.adoc"
cp -v "$in/changelog.adoc" "$out/modules/interface-spec/partials/changelog.adoc"
cp -v "$in"/*.did "$in"/*.cddl "$out/modules/interface-spec/attachments"
cp -v "$in"/*.did "$in"/*.cddl "$out/modules/interface-spec/examples"


