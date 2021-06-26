"""
A Python module containing a bunch of random functions that I have written over
the years.
"""

# Import standard modules ...
import sys

# Import sub-functions ...
from .convert_bytes_to_pretty_bytes import convert_bytes_to_pretty_bytes
from .convert_pretty_bytes_to_bytes import convert_pretty_bytes_to_bytes
from .convert_spreadsheet_to_datetime import convert_spreadsheet_to_datetime
from .convert_spreadsheet_to_unix import convert_spreadsheet_to_unix
from .dot2png import dot2png
from .download import download
from .download_file import download_file
from .download_header import download_header
from .download_stream import download_stream
from .download_text import download_text
from .exiftool import exiftool
from .find_instances_of_a_file import find_instances_of_a_file
from .find_integer_divisors import find_integer_divisors
from .find_program_version import find_program_version
from .generate_password import generate_password
from .generate_random_stub import generate_random_stub
from .getatime import getatime
from .getctime import getctime
from .getmtime import getmtime
from .gifsicle import gifsicle
from .git_commits import git_commits
from .hostname import hostname
from .interpolate import interpolate
from .jpegtran import jpegtran
from .jpg2png import jpg2png
from .load_EXIF import load_EXIF
from .load_EXIF1 import load_EXIF1
from .load_EXIF2 import load_EXIF2
from .load_GPS_EXIF import load_GPS_EXIF
from .load_GPS_EXIF1 import load_GPS_EXIF1
from .load_GPS_EXIF2 import load_GPS_EXIF2
from .make_path_safe import make_path_safe
from .now import now
from .optimize_image import optimize_image
from .optipng import optipng
from .paeth_filter import paeth_filter
from .png2jpg import png2jpg
from .remove_almost_empty_directories import remove_almost_empty_directories
from .return_file_list import return_file_list
from .return_folder_list import return_folder_list
from .return_folder_size import return_folder_size
from .return_hash_of_GZ import return_hash_of_GZ
from .return_hash_of_MP4 import return_hash_of_MP4
from .return_link_list import return_link_list
from .save_array_as_image import save_array_as_image
from .save_array_as_PNG import save_array_as_PNG
from .save_array_as_PPM import save_array_as_PPM
from .save_file_if_needed import save_file_if_needed
from .serializer import serializer
from .sha256 import sha256
from .sha512 import sha512
from .start_session import start_session
from .stat import stat
from .tar import tar
from .xz import xz

# Ensure that this module is only imported by Python 3.x ...
if sys.version_info.major != 3:
    raise Exception("the Python module \"pyguymer3\" must only be used with Python 3.x, if you want a Python 2.x version then use \"pyguymer\" instead") from None
