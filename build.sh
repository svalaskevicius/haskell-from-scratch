#!/bin/sh

remarker -s haskell_from_scratch.md build

odpdown  --break-master Impress --content-master Impress1 -n haskell_from_scratch.md template.odp  haskell_from_scratch.odp


