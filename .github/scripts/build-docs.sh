#!/usr/bin/env bash
# Build the manual as a standalone site in _site/. The package must already be
# installed, since the examples in the manual run against it.
set -euo pipefail

cd "$(dirname "$0")/../.."

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
