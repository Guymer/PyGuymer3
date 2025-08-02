#!/usr/bin/env python3

"""
A Python sub-module containing functions relating to`OpenStreetMap [2]_.

Notes
-----
Copyright 2017 Thomas Guymer [1]_

References
----------
.. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
.. [2] OpenStreetMap, https://www.openstreetmap.org
"""

# Import sub-functions ...
from .deg2num import deg2num
from .hfov import hfov
from .num2deg import num2deg
from .res import res
from .tile import tile
from .tiles import tiles
from .vfov import vfov
from .zoom import zoom
