#!/usr/bin/env bash

# Run scripts ...
python3.11 generateGlobeTiles.py                                                \
    --maximum-elevation-interval 1000
python3.11 generateGshhgTiles.py                                                \
    --resolutions c l i h f
python3.11 generateNeTiles.py                                                   \
    --elevation-band-interval 250                                               \
    --maximum-elevation-interval 1000                                           \
    --resolutions 10m 50m 110m
python3.11 generateOsTerrainTiles.py                                            \
    --maximum-elevation-interval 250
