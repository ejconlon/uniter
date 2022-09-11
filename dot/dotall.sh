#!/bin/bash

# Renders each dot file here as png. Needs graphviz installed.

set -eu

for INPUT in dot/output/*.dot
do
  echo "Rendering graphviz ${INPUT}"
  OUTPUT=${INPUT%.dot}.png
  dot -Tpng ${INPUT} -o${OUTPUT}
done
