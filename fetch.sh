#!/bin/sh

rm -f internal_research.json
wget "65.21.5.41/kckeyworddata/internal_research.json" 
mv internal_research.json data/internal_research.json