#!/bin/sh

pandoc --slide-level=2 -f markdown -t revealjs -o .contents.html haskell_from_scratch.md

php haskell_from_scratch.php > haskell_from_scratch.html

