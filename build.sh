#!/bin/sh

remarker -s haskell_from_scratch.md build

# cat haskell_from_scratch.md | perl -p -e 's/^\.(center|pull-right)\[(.*?)\]$/$2/' > _haskell_from_scratch.md
odpdown  --break-master Impress --content-master Impress1 -n haskell_from_scratch.md template.odp  haskell_from_scratch.odp
rm -f _haskell_from_scratch.md

