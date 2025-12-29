#!/usr/bin/env python3

"""
A Python sub-module containing a bunch of random geo-related functions that I
have written over the years.

Notes
-----
See the following two sources of documentation from GIS software to read a bit
more about buffering:

* `ArcGIS buffering <https://desktop.arcgis.com/en/arcmap/latest/tools/analysis-toolbox/buffer.htm>`_
* `QGIS buffering <https://qgis.org/pyqgis/3.22/core/QgsGeometry.html#qgis.core.QgsGeometry.buffer>`_

To clarify some the geometry data types:

* A ``shapely.geometry.point.Point`` is made up of a
  ``shapely.coords.CoordinateSequence`` of 1 ``tuple`` of a (lon,lat) pair.
* A ``shapely.geometry.polygon.LinearRing`` is made up of a
  ``shapely.coords.CoordinateSequence`` of N ``tuple`` of (lon,lat) pairs.
* A ``shapely.geometry.linestring.LineString`` is made up of a
  ``shapely.coords.CoordinateSequence`` of N ``tuple`` of (lon,lat) pairs.
* A ``shapely.geometry.polygon.Polygon`` is made up of a
  ``shapely.geometry.polygon.LinearRing`` exterior (and *maybe* N
  ``shapely.geometry.polygon.LinearRing`` interiors).
* A ``shapely.geometry.multipoint.MultiPoint`` is made up of N
  ``shapely.geometry.point.Point``.
* A ``shapely.geometry.multilinestring.MultiLineString`` is made up of N
  ``shapely.geometry.linestring.LineString``.
* A ``shapely.geometry.multipolygon.MultiPolygon`` is made up of N
  ``shapely.geometry.polygon.Polygon``.

The basic call graphs for :func:`pyguymer3.geo.buffer` and
:func:`pyguymer3.geo.fillin` are:

* :func:`pyguymer3.geo.buffer` calls:

    * :func:`pyguymer3.geo.bufferSrc.buffer_CoordinateSequence` (this is the
      only function that actually does any buffering, as evidenced by it not
      calling any other ``bufferSrc`` functions)

        * :func:`pyguymer3.geo._buffer_points_crudely`
        * :func:`pyguymer3.geo._points2polys`
        * :func:`pyguymer3.geo.fillin`

    * :func:`pyguymer3.geo.bufferSrc.buffer_LinearRing` calls:

        * :func:`pyguymer3.geo.bufferSrc.buffer_CoordinateSequence`

    * :func:`pyguymer3.geo.bufferSrc.buffer_LineString` calls:

        * :func:`pyguymer3.geo.bufferSrc.buffer_CoordinateSequence`

    * :func:`pyguymer3.geo.bufferSrc.buffer_MultiLineString` calls:

        * :func:`pyguymer3.geo.bufferSrc.buffer_LineString`

    * :func:`pyguymer3.geo.bufferSrc.buffer_MultiPoint` calls:

        * :func:`pyguymer3.geo.bufferSrc.buffer_Point`

    * :func:`pyguymer3.geo.bufferSrc.buffer_MultiPolygon` calls:

        * :func:`pyguymer3.geo.bufferSrc.buffer_Polygon`

    * :func:`pyguymer3.geo.bufferSrc.buffer_Point` calls:

        * :func:`pyguymer3.geo.bufferSrc.buffer_CoordinateSequence`

    * :func:`pyguymer3.geo.bufferSrc.buffer_Polygon` calls:

        * :func:`pyguymer3.geo.bufferSrc.buffer_LinearRing`

* :func:`pyguymer3.geo.fillin`

    * :func:`pyguymer3.geo.fillinSrc.fillin_CoordinateSequence` (this is the
      only function that actually does any filling in, as evidenced by it not
      calling any other ``fillinSrc`` functions)

        * :func:`pyguymer3.geo.calc_dist_between_two_locs`
        * :func:`pyguymer3.geo.great_circle`

    * :func:`pyguymer3.geo.fillinSrc.fillin_LinearRing` calls:

        * :func:`pyguymer3.geo.fillinSrc.fillin_CoordinateSequence`

    * :func:`pyguymer3.geo.fillinSrc.fillin_LineString` calls:

        * :func:`pyguymer3.geo.fillinSrc.fillin_CoordinateSequence`

    * :func:`pyguymer3.geo.fillinSrc.fillin_MultiLineString` calls:

        * :func:`pyguymer3.geo.fillinSrc.fillin_LineString`

    * :func:`pyguymer3.geo.fillinSrc.fillin_MultiPolygon` calls:

        * :func:`pyguymer3.geo.fillinSrc.fillin_Polygon`

    * :func:`pyguymer3.geo.fillinSrc.fillin_Polygon` calls:

        * :func:`pyguymer3.geo.fillinSrc.fillin_LinearRing`

Copyright 2017 Thomas Guymer [1]_

References
----------
.. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
"""

# Import sub-functions ...
from ._add_antarcticIceShelves import _add_antarcticIceShelves
from ._add_background import _add_background
from ._add_bathymetry import _add_bathymetry
from ._add_coastlines import _add_coastlines
from ._add_elevation import _add_elevation
from ._add_glaciatedAreas import _add_glaciatedAreas
from ._add_global_axis import _add_global_axis
from ._add_horizontal_gridlines import _add_horizontal_gridlines
from ._add_lakes import _add_lakes
from ._add_land import _add_land
from ._add_minorIslands import _add_minorIslands
from ._add_playas import _add_playas
from ._add_railroads import _add_railroads
from ._add_reefs import _add_reefs
from ._add_rivers import _add_rivers
from ._add_roads import _add_roads
from ._add_topDown_axis import _add_topDown_axis
from ._add_urbanAreas import _add_urbanAreas
from ._add_vertical_gridlines import _add_vertical_gridlines
from ._area import _area
from ._buffer_points_crudely import _buffer_points_crudely
from ._debug import _debug
from ._points2polys import _points2polys
from .add_annotation import add_annotation
from .add_axis import add_axis
from .add_Cartopy_tiles import add_Cartopy_tiles
from .add_GLOBE_tiles import add_GLOBE_tiles
from .add_GSHHG_map import add_GSHHG_map
from .add_GSHHG_tiles import add_GSHHG_tiles
from .add_map_background import add_map_background
from .add_NE_map import add_NE_map
from .add_NE_tiles import add_NE_tiles
from .add_OSterrain_tiles import add_OSterrain_tiles
from .area import area
from .buffer import buffer
from .calc_angle_between_two_locs import calc_angle_between_two_locs
from .calc_dist_between_two_locs import calc_dist_between_two_locs
from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
from .check import check
from .clean import clean
from .clipLatitude import clipLatitude
from .create_image_of_points import create_image_of_points
from .create_map_of_points import create_map_of_points
from .en2ll import en2ll
from .extract_lines import extract_lines
from .extract_points import extract_points
from .extract_polys import extract_polys
from .fillin import fillin
from .find_middle_of_great_circle import find_middle_of_great_circle
from .find_middle_of_locs import find_middle_of_locs
from .find_min_max_dist_bearing import find_min_max_dist_bearing
from .find_point_on_great_circle import find_point_on_great_circle
from .getRecordAttribute import getRecordAttribute
from .great_circle import great_circle
from .ll2en import ll2en
from .ll2mer import ll2mer
from .max_dist import max_dist
from .mer2ll import mer2ll
from .min_dist import min_dist
from .wrapLongitude import wrapLongitude
