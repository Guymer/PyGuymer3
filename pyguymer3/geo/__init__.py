"""
A Python sub-module containing a bunch of random geo-related functions that I
have written over the years.

Notes
-----
Filling example: stand on North Pole, go X degrees in all directions, all points
in circle will have same latitude, which will get simplified to a Euclidean
rectangle, which will get drawn as a rhombus, not a circle, therefore need to
fill in the gaps between the points to keep the Geodesic shape

Simplify example: a rectangle with multiple points on one edge, even if the
points are all on the same line, if they are not in the correct order then it
will break, so they need to be removed with .simplify(0)

Spike example: a rectangle with a re-entrant spike, that is valid, becomes
invalid if there are multiple points on the spike (a Polygon can only touch
itself once)
"""

# Import sub-functions ...
from ._add_antarcticIceShelves import _add_antarcticIceShelves
from ._add_background import _add_background
from ._add_bathymetry import _add_bathymetry
from ._add_glaciatedAreas import _add_glaciatedAreas
from ._add_lakes import _add_lakes
from ._add_land import _add_land
from ._add_minorIslands import _add_minorIslands
from ._add_playas import _add_playas
from ._add_railroads import _add_railroads
from ._add_reefs import _add_reefs
from ._add_rivers import _add_rivers
from ._add_roads import _add_roads
from ._buffer_points_crudely import _buffer_points_crudely
from ._debug import _debug
from ._earthA import _earthA
from ._earthB import _earthB
from ._earthC import _earthC
from ._earthD import _earthD
from ._earthE import _earthE
from ._earthF import _earthF
from ._earthG import _earthG
from ._points2poly import _points2poly
from ._posts2panel import _posts2panel
from .add_horizontal_gridlines import add_horizontal_gridlines
from .add_map_background import add_map_background
from .add_map_underlay import add_map_underlay
from .add_vertical_gridlines import add_vertical_gridlines
from .buffer import buffer
from .buffer_CoordinateSequence import buffer_CoordinateSequence
from .buffer_LinearRing import buffer_LinearRing
from .buffer_LineString import buffer_LineString
from .buffer_MultiLineString import buffer_MultiLineString
from .buffer_MultiPolygon import buffer_MultiPolygon
from .buffer_Point import buffer_Point
from .buffer_Polygon import buffer_Polygon
from .calc_angle_between_two_locs import calc_angle_between_two_locs
from .calc_dist_between_two_locs import calc_dist_between_two_locs
from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
from .en2ll import en2ll
from .extract_lines import extract_lines
from .extract_polys import extract_polys
from .fillin import fillin
from .fillin_CoordinateSequence import fillin_CoordinateSequence
from .fillin_LinearRing import fillin_LinearRing
from .fillin_MultiPolygon import fillin_MultiPolygon
from .fillin_Polygon import fillin_Polygon
from .find_middle_of_great_circle import find_middle_of_great_circle
from .find_point_on_great_circle import find_point_on_great_circle
from .great_circle import great_circle
from .remap import remap
