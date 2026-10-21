#!/usr/bin/env bash
# Build the manual as a standalone site in _site/.
set -euo pipefail

cd "$(dirname "$0")/.."

# The examples in the manual require parser-combinator/..., so make this
# checkout the parser-combinator collection, whatever its directory is named.
# PLTCOLLECTS is searched before installed packages, so this wins over an
# installed copy of the package.
collects=$(mktemp -d)
trap 'rm -rf "$collects"' EXIT
ln -s "$PWD" "$collects/parser-combinator"
export PLTCOLLECTS="$collects:"

rm -rf _site

# Point links into the Racket docs at docs.racket-lang.org, instead of at the
# local installation.
raco scribble --htmls \
  --dest . --dest-name _site \
  --redirect-main https://docs.racket-lang.org/ \
  ++main-xref-in \
  scribblings/parser-combinator.scrbl

# Stop Pages from running Jekyll over the built site.
touch _site/.nojekyll
