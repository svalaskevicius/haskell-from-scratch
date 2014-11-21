#!/bin/sh

pandoc --slide-level=2 --highlight-style zenburn -f markdown -t revealjs -o .contents.html haskell_from_scratch.md

php haskell_from_scratch.php > index.html

