#!/usr/bin/env bash

# Source run control ...
source "${BASH_ENV}" || exit 1

# Check that non-standard programs are installed. "standard" programs are
# anything that is specified in the POSIX.1-2008 standard (and the IEEE Std
# 1003.1 standard) or that is a BASH builtin command. Therefore, "non-standard"
# programs are anything that does not appear on the following two lists:
#   * https://pubs.opengroup.org/onlinepubs/9699919799/idx/utilities.html
#   * https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html

# Create short-hand ...
NCHILD=6                                                                        # [#]

# Generate one-off-tiles ...
python3.11 generateMissingTile.py &> generateMissingTile.log

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
python3.11 generateGlobeGshhgTiles.py                                           \
    --number-of-children ${NCHILD} &> generateGlobeGshhgTiles.log
python3.11 generateGlobeNeTiles.py                                              \
    --number-of-children ${NCHILD} &> generateGlobeNeTiles.log
