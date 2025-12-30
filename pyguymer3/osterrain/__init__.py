#!/usr/bin/env python3

"""
A Python sub-module containing functions relating to the "OS Terrain 50" dataset
[2]_.

Notes
-----
Copyright 2017 Thomas Guymer [1]_

References
----------
.. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
.. [2] "OS Terrain 50" dataset, https://www.ordnancesurvey.co.uk/products/os-terrain-50
"""

# Import sub-functions ...
from .findExtent import findExtent
from .loadASCIIcontents import loadASCIIcontents
from .loadASCIIheader import loadASCIIheader
