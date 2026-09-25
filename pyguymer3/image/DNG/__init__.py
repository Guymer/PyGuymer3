#!/usr/bin/env python3

"""
This sub-module is a native Python implementation of a parser for DNG metadata
and, perhaps more importantly, a hasher of the binary image data within - thus
allowing two different DNGs to be compared if the binary image data within them
is the same (i.e., one is an original and one contains extra XMP editting data
inserted by Adobe Lightroom which has changed the overall hash of the DNG file
but not the binary image data within).

Notes
-----
Copyright 2017 Thomas Guymer [1]_

References
----------
.. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3

Examples
--------
The below code will print out the entire dictionary for you.

>>> import pyguymer3
>>> import pyguymer3.image
>>> info = parseDNGsrc.parseFile("/path/to/image.dng")
>>> import json
>>> print(json.dumps(info, ensure_ascii = False, indent = 4, sort_keys = True))

"""

# Import sub-functions ...
from .parseDirEntries import parseDirEntries
from .processDirEntries import processDirEntries
from .tagId2tagName import tagId2tagName
from .typeId2typeName import typeId2typeName
