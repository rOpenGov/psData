#!/bin/bash
pandoc --variable=author:"Jeffrey B.Arnold" \
    --variable=title:"DataFrameConstr" \
    --variable=date:"$DATE" \
    --toc \
    -o inst/doc/README.pdf \
    README.md



