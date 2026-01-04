#!/usr/bin/env bash

# Create short-hand ...
NCHILD=6                                                                        # [#]

# Generate vector tiles ...
python3.11 generateGshhgTiles.py                                                \
    --number-of-children ${NCHILD} &> generateGshhgTiles.log
python3.11 generateNeTiles.py                                                   \
    --number-of-children ${NCHILD} &> generateNeTiles.log

# Generate raster tiles ...
python3.11 generateGlobeTiles.py                                                \
    --number-of-children ${NCHILD} &> generateGlobeTiles.log
python3.11 generateOsTerrainTiles.py                                            \
    --number-of-children ${NCHILD} &> generateOsTerrainTiles.log

# Generate combined raster+vector tiles ...
python3.11 generateGlobeNeTiles.py                                              \
    --number-of-children ${NCHILD} &> generateGlobeNeTiles.log
