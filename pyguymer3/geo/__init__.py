#!/usr/bin/env python3

"""
A Python sub-module containing a bunch of random geo-related functions that I have written over the years.

Notes
-----

This is quite a complicated sub-module; here are some of my development notes.

Documentation
^^^^^^^^^^^^^

See the following two sources of documentation from GIS software:

* ArcGIS buffering [1]_
* QGIS buffering [2]_

Data Types
^^^^^^^^^^

* A ``shapely.geometry.point.Point`` is made up of a ``shapely.coords.CoordinateSequence`` of 1 ``tuple`` of a (lon,lat) pair.
* A ``shapely.geometry.polygon.LinearRing`` is made up of a ``shapely.coords.CoordinateSequence`` of N ``tuple`` of (lon,lat) pairs.
* A ``shapely.geometry.linestring.LineString`` is made up of a ``shapely.coords.CoordinateSequence`` of N ``tuple`` of (lon,lat) pairs.
* A ``shapely.geometry.polygon.Polygon`` is made up of a ``shapely.geometry.polygon.LinearRing`` exterior (and *maybe* N ``shapely.geometry.polygon.LinearRing`` interiors).
* A ``shapely.geometry.multipoint.MultiPoint`` is made up of N ``shapely.geometry.point.Point``.
* A ``shapely.geometry.multilinestring.MultiLineString`` is made up of N ``shapely.geometry.linestring.LineString``.
* A ``shapely.geometry.multipolygon.MultiPolygon`` is made up of N ``shapely.geometry.polygon.Polygon``.

Call Graph
^^^^^^^^^^

Public functions:

* ``buffer()`` calls:

    * ``bufferSrc.buffer_CoordinateSequence()`` (this is the only function that actually does any buffering, as evidenced by it not calling any other ``bufferSrc`` functions)

        * ``_buffer_points_crudely()``
        * ``_points2polys()``
        * ``fillin()``

    * ``bufferSrc.buffer_LinearRing()`` calls:

        * ``bufferSrc.buffer_CoordinateSequence()``

    * ``bufferSrc.buffer_LineString()`` calls:

        * ``bufferSrc.buffer_CoordinateSequence()``

    * ``bufferSrc.buffer_MultiLineString()`` calls:

        * ``bufferSrc.buffer_LineString()``

    * ``bufferSrc.buffer_MultiPoint()`` calls:

        * ``bufferSrc.buffer_Point()``

    * ``bufferSrc.buffer_MultiPolygon()`` calls:

        * ``bufferSrc.buffer_Polygon()``

    * ``bufferSrc.buffer_Point()`` calls:

        * ``bufferSrc.buffer_CoordinateSequence()``

    * ``bufferSrc.buffer_Polygon()`` calls:

        * ``bufferSrc.buffer_LinearRing()``

* ``fillin()``

    * ``fillinSrc.fillin_CoordinateSequence()`` (this is the only function that actually does any filling in, as evidenced by it not calling any other ``fillinSrc`` functions)

        * ``calc_dist_between_two_locs()``
        * ``great_circle()``

    * ``fillinSrc.fillin_LinearRing()`` calls:

        * ``fillinSrc.fillin_CoordinateSequence()``

    * ``fillinSrc.fillin_LineString()`` calls:

        * ``fillinSrc.fillin_CoordinateSequence()``

    * ``fillinSrc.fillin_MultiLineString()`` calls:

        * ``fillinSrc.fillin_LineString()``

    * ``fillinSrc.fillin_MultiPolygon()`` calls:

        * ``fillinSrc.fillin_Polygon()``

    * ``fillinSrc.fillin_Polygon()`` calls:

        * ``fillinSrc.fillin_LinearRing()``

References
----------
.. [1] ArcGIS buffering, https://desktop.arcgis.com/en/arcmap/latest/tools/analysis-toolbox/buffer.htm
.. [2] QGIS buffering, https://qgis.org/pyqgis/3.22/core/QgsGeometry.html#qgis.core.QgsGeometry.buffer
"""

# Import sub-functions ...
from ._add_antarcticIceShelves import _add_antarcticIceShelves
from ._add_background import _add_background
from ._add_bathymetry import _add_bathymetry
from ._add_elevation import _add_elevation
from ._add_glaciatedAreas import _add_glaciatedAreas
from ._add_lakes import _add_lakes
from ._add_land import _add_land
from ._add_minorIslands import _add_minorIslands
from ._add_playas import _add_playas
from ._add_railroads import _add_railroads
from ._add_reefs import _add_reefs
from ._add_rivers import _add_rivers
from ._add_roads import _add_roads
from ._add_urbanAreas import _add_urbanAreas
from ._area import _area
from ._buffer_points_crudely import _buffer_points_crudely
from ._debug import _debug
from ._points2polys import _points2polys
from .add_annotation import add_annotation
from .add_coastlines import add_coastlines
from .add_horizontal_gridlines import add_horizontal_gridlines
from .add_map_background import add_map_background
from .add_map_underlay import add_map_underlay
from .add_top_down_axis import add_top_down_axis
from .add_vertical_gridlines import add_vertical_gridlines
from .area import area
from .buffer import buffer
from .calc_angle_between_two_locs import calc_angle_between_two_locs
from .calc_dist_between_two_locs import calc_dist_between_two_locs
from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
from .check import check
from .clean import clean
from .clipLatitude import clipLatitude
from .en2ll import en2ll
from .extract_lines import extract_lines
from .extract_points import extract_points
from .extract_polys import extract_polys
from .fillin import fillin
from .find_middle_of_great_circle import find_middle_of_great_circle
from .find_point_on_great_circle import find_point_on_great_circle
from .getRecordAttribute import getRecordAttribute
from .great_circle import great_circle
from .ll2en import ll2en
from .wrapLongitude import wrapLongitude
