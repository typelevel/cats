#!/bin/bash

set -eux

gitbook="node_modules/gitbook-cli/bin/gitbook.js"
tut="docs/target/scala-2.11/tut"

# Brings AUTHORS.md, CHANGES.md, ...
cp *.md $tut/

# This removes badges from the README.
# Because badges are in svg, leaving them adds an extra (heavy) dependeny on `svgexport`
echo -e "# [Cats](https://github.com/typelevel/cats) \n$(sed -ne '/Overview/,$p' README.md)" \
  > $tut/README.md

if ! test -e $gitbook; then
  npm install gitbook
  npm install gitbook-cli
  sudo apt-get install -y calibre # Required for ebook stuff
fi

$gitbook build $tut book
$gitbook pdf   $tut book/ebook.pdf
$gitbook epub  $tut book/ebook.epub
$gitbook mobi  $tut book/ebook.mobi

exit 0
