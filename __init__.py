"""
A Python module containing a bunch of random functions that I have written over
the years.
"""

# Import standard modules ...
import sys

# Import sub-functions ...
from .add_map_background import add_map_background
from .buffer_multipolygon import buffer_multipolygon
from .buffer_point import buffer_point
from .buffer_points import buffer_points
from .buffer_polygon import buffer_polygon
from .calc_angle_between_two_locs import calc_angle_between_two_locs
from .calc_dist_between_two_locs import calc_dist_between_two_locs
from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
from .convert_bytes_to_pretty_bytes import convert_bytes_to_pretty_bytes
from .convert_pretty_bytes_to_bytes import convert_pretty_bytes_to_bytes
from .convert_spreadsheet_to_unix import convert_spreadsheet_to_unix
from .does_media_have_RTP_hints import does_media_have_RTP_hints
from .download import download
from .download_file import download_file
from .download_header import download_header
from .download_stream import download_stream
from .download_text import download_text
from .exiftool import exiftool
from .find_instances_of_a_file import find_instances_of_a_file
from .find_integer_divisors import find_integer_divisors
from .find_middle_of_great_circle import find_middle_of_great_circle
from .find_point_on_great_circle import find_point_on_great_circle
from .find_program_version import find_program_version
from .generate_password import generate_password
from .generate_random_stub import generate_random_stub
from .gifsicle import gifsicle
from .hostname import hostname
from .interpolate import interpolate
from .is_moov_at_beginning_of_MP4 import is_moov_at_beginning_of_MP4
from .jpegtran import jpegtran
from .load_EXIF import load_EXIF
from .load_EXIF1 import load_EXIF1
from .load_EXIF2 import load_EXIF2
from .load_GPS_EXIF import load_GPS_EXIF
from .load_GPS_EXIF1 import load_GPS_EXIF1
from .load_GPS_EXIF2 import load_GPS_EXIF2
from .make_path_safe import make_path_safe
from .optipng import optipng
from .paeth_filter import paeth_filter
from .parse_CLPI_file import parse_CLPI_file
from .parse_MPLS_file import parse_MPLS_file
from .print_MP4_atoms import print_MP4_atoms
from .remove_almost_empty_directories import remove_almost_empty_directories
from .return_dict_of_bluray_playlists import return_dict_of_bluray_playlists
from .return_dict_of_ISO_audio_streams import return_dict_of_ISO_audio_streams
from .return_dict_of_ISO_subtitle_streams import return_dict_of_ISO_subtitle_streams
from .return_dict_of_ISO_tracks import return_dict_of_ISO_tracks
from .return_dict_of_media_audio_streams import return_dict_of_media_audio_streams
from .return_dict_of_media_subtitle_streams import return_dict_of_media_subtitle_streams
from .return_dict_of_media_video_streams import return_dict_of_media_video_streams
from .return_file_list import return_file_list
from .return_file_list_with_warnings import return_file_list_with_warnings
from .return_hash_of_MP4 import return_hash_of_MP4
from .return_ISO_palette import return_ISO_palette
from .return_media_duration import return_media_duration
from .return_MP4_video_level import return_MP4_video_level
from .return_MP4_video_profile import return_MP4_video_profile
from .return_subtitle_extent import return_subtitle_extent
from .return_video_bit_depth import return_video_bit_depth
from .return_video_crop_parameters import return_video_crop_parameters
from .return_video_display_aspect_ratio import return_video_display_aspect_ratio
from .return_video_frame_rate import return_video_frame_rate
from .return_video_height import return_video_height
from .return_video_pixel_aspect_ratio import return_video_pixel_aspect_ratio
from .return_video_ratios import return_video_ratios
from .return_video_rotation import return_video_rotation
from .return_video_source_aspect_ratio import return_video_source_aspect_ratio
from .return_video_width import return_video_width
from .save_array_as_image import save_array_as_image
from .save_array_as_PNG import save_array_as_PNG
from .save_array_as_PPM import save_array_as_PPM
from .save_file_if_needed import save_file_if_needed
from .simplify_poly import simplify_poly
from .start_session import start_session
from .yuv2rgb import yuv2rgb

# Ensure that this module is only imported by Python 3.x ...
if sys.version_info.major != 3:
    raise Exception("the Python module \"pyguymer3\" must only be used with Python 3.x, if you want a Python 2.x version then use \"pyguymer\" instead")
