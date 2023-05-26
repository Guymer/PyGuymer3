#!/usr/bin/env python3

"""
This sub-module is a native Python implementation of a parser for Blu-ray CLPI files.

Notes
-----
It has used an excellent CLPI Wiki [1]_ and was initially provided by lonecrane [2]_ - thank you very much!

References
----------
.. [1] CLPI Wiki, https://github.com/lw/BluRay/wiki/CLPI
.. [2] lonecrane on GitHub, https://github.com/lonecrane

Examples
--------
The below code will print out the entire dictionary for you.

>>> import pyguymer3
>>> import pyguymer3.media
>>> info = pyguymer3.media.parse_CLPI_file("/path/to/blu-ray", 200)
>>> import json
>>> print(json.dumps(info, ensure_ascii = False, indent = 4, sort_keys = True))

"""

# Import sub-functions ...
from .load_ATCSequence import load_ATCSequence
from .load_ClipInfo import load_ClipInfo
from .load_ClipMark import load_ClipMark
from .load_CPI import load_CPI
from .load_ExtensionData import load_ExtensionData
from .load_header import load_header
from .load_ProgramInfo import load_ProgramInfo
from .load_SequenceInfo import load_SequenceInfo
from .load_STCSequence import load_STCSequence
from .load_StreamCodingInfo import load_StreamCodingInfo
from .load_TSTypeInfoBlock import load_TSTypeInfoBlock
