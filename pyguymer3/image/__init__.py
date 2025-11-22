#!/usr/bin/env python3

"""
A Python sub-module containing a bunch of random image-related functions that I
have written over the years. If you want to use some of these functions directly
from the command line then you can run ``python3.12 -m pyguymer3.image --help``
to see what is available.

Notes
-----
Copyright 2017 Thomas Guymer [1]_

References
----------
.. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
"""

# Import sub-functions ...
from .__exifread__ import __exifread__
from .__exiftool__ import __exiftool__
from .__pillow__ import __pillow__
from .dict2exif import dict2exif
from .dot2png import dot2png
from .EXIF_datetime import EXIF_datetime
from .exiftool import exiftool
from .gifsicle import gifsicle
from .image2gif import image2gif
from .image2jpg import image2jpg
from .image2png import image2png
from .image2webp import image2webp
from .is_image_animated import is_image_animated
from .jpegtran import jpegtran
from .load_EXIF import load_EXIF
from .load_GPS_EXIF import load_GPS_EXIF
from .load_GPS_EXIF1 import load_GPS_EXIF1
from .load_GPS_EXIF2 import load_GPS_EXIF2
from .makePng import makePng
from .manuallyOptimisePng import manuallyOptimisePng
from .optimise_image import optimise_image
from .optipng import optipng
from .return_image_bit_depth import return_image_bit_depth
from .return_image_format import return_image_format
from .return_image_height import return_image_height
from .return_image_mode import return_image_mode
from .return_image_size import return_image_size
from .return_image_width import return_image_width
from .returnPngInfo import returnPngInfo
from .rgb2hlsComp import rgb2hlsComp
from .rgb2name import rgb2name
from .rgb2rgbComp import rgb2rgbComp
from .save_array_as_image import save_array_as_image
from .save_array_as_PGM import save_array_as_PGM
from .save_array_as_PNG import save_array_as_PNG
from .save_array_as_PPM import save_array_as_PPM
