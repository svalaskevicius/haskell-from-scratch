#!/bin/sh

pandoc --slide-level=2 -f markdown -t revealjs -o haskell_from_scratch.html haskell_from_scratch.md

php -r '$tpl = file_get_contents("hfs_tpl.html"); $data = file_get_contents("haskell_from_scratch.html"); file_put_contents("haskell.html", str_replace("##SLIDE_CONTENTS##", $data, $tpl));'
