#!/usr/bin/env bash

# Run scripts ...
python3.11 generateGlobeTiles.py &> generateGlobeTiles.log
python3.11 generateGshhgTiles.py &> generateGshhgTiles.log
python3.11 generateNeTiles.py &> generateNeTiles.log
python3.11 generateOsTerrainTiles.py &> generateOsTerrainTiles.log
