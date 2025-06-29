#!/usr/bin/env python3

"""
A Python sub-module containing a bunch of random image-related functions that I
have written over the years.

Notes
-----
Copyright 2017 Thomas Guymer [1]_

References
----------
.. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
"""

# Import sub-functions ...
from .dict2exif import dict2exif
from .dot2png import dot2png
from .EXIF_datetime import EXIF_datetime
from .exiftool import exiftool
from .gifsicle import gifsicle
from .image2gif import image2gif
from .image2jpg import image2jpg
from .image2png import image2png
from .image2webp import image2webp
from .jpegtran import jpegtran
from .load_EXIF import load_EXIF
from .load_EXIF1 import load_EXIF1
from .load_EXIF2 import load_EXIF2
from .load_GPS_EXIF import load_GPS_EXIF
from .load_GPS_EXIF1 import load_GPS_EXIF1
from .load_GPS_EXIF2 import load_GPS_EXIF2
from .makePng import makePng
from .manuallyOptimisePng import manuallyOptimisePng
from .optimize_image import optimize_image
from .optipng import optipng
from .return_image_size import return_image_size
from .save_array_as_image import save_array_as_image
from .save_array_as_PNG import save_array_as_PNG
from .save_array_as_PPM import save_array_as_PPM
