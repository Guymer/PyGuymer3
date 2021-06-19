"""
A Python module containing a bunch of random geo-related functions that I have
written over the years.
"""

# Import sub-functions ...
from ._buffer_points import _buffer_points
from ._buffer_points_crudely import _buffer_points_crudely
from ._fix_ring import _fix_ring
from .add_map_background import add_map_background
from .buffer import buffer
from .buffer_CoordinateSequence import buffer_CoordinateSequence
from .buffer_LinearRing import buffer_LinearRing
from .buffer_MultiPolygon import buffer_MultiPolygon
from .buffer_Point import buffer_Point
from .buffer_Polygon import buffer_Polygon
from .calc_angle_between_two_locs import calc_angle_between_two_locs
from .calc_dist_between_two_locs import calc_dist_between_two_locs
from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
from .en2ll import en2ll
from .find_middle_of_great_circle import find_middle_of_great_circle
from .find_point_on_great_circle import find_point_on_great_circle
from .simplify_poly import simplify_poly
