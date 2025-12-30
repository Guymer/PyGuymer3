#!/usr/bin/env bash

# Run scripts ...
python3.11 generateGlobeTiles.py --number-of-children 4
python3.11 generateGshhgTiles.py --number-of-children 4
python3.11 generateNeTiles.py --number-of-children 4
python3.11 generateOsTerrainTiles.py --number-of-children 4
