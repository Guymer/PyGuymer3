# `pyguymer3.geo`

This is a Python 3.X sub-module containing a bunch of random functions that I have written over the years which are geo-related.

## Documentation

See the following two sources of documentation from GIS software:

* [ArcGIS](https://desktop.arcgis.com/en/arcmap/latest/tools/analysis-toolbox/buffer.htm)
* [QGIS](https://qgis.org/pyqgis/3.22/core/QgsGeometry.html#qgis.core.QgsGeometry.buffer)

## Data Types

* A `shapely.geometry.point.Point` is made up of a `shapely.coords.CoordinateSequence` of 1 `tuple` of a (lon,lat) pair.
* A `shapely.geometry.polygon.LinearRing` is made up of a `shapely.coords.CoordinateSequence` of N `tuple`s of (lon,lat) pairs.
* A `shapely.geometry.linestring.LineString` is made up of a `shapely.coords.CoordinateSequence` of N `tuple`s of (lon,lat) pairs.
* A `shapely.geometry.polygon.Polygon` is made up of a `shapely.geometry.polygon.LinearRing` exterior (and *maybe* N `shapely.geometry.polygon.LinearRing` interiors).
* A `shapely.geometry.multipoint.MultiPoint` is made up of N `shapely.geometry.point.Point`s.
* A `shapely.geometry.multilinestring.MultiLineString` is made up of N `shapely.geometry.linestring.LineString`s.
* A `shapely.geometry.multipolygon.MultiPolygon` is made up of N `shapely.geometry.polygon.Polygon`s.

## Call Graph

Public functions:

* `buffer()` calls:
    * `bufferSrc.buffer_CoordinateSequence()` (this is the only function that actually does any buffering, as evidenced by it not calling any other `bufferSrc` functions)
        * `_buffer_points_crudely()`
        * `_points2polys()`
        * `fillin()`
    * `bufferSrc.buffer_LinearRing()` calls:
        * `bufferSrc.buffer_CoordinateSequence()`
    * `bufferSrc.buffer_LineString()` calls:
        * `bufferSrc.buffer_CoordinateSequence()`
    * `bufferSrc.buffer_MultiLineString()` calls:
        * `bufferSrc.buffer_LineString()`
    * `bufferSrc.buffer_MultiPoint()` calls:
        * `bufferSrc.buffer_Point()`
    * `bufferSrc.buffer_MultiPolygon()` calls:
        * `bufferSrc.buffer_Polygon()`
    * `bufferSrc.buffer_Point()` calls:
        * `bufferSrc.buffer_CoordinateSequence()`
    * `bufferSrc.buffer_Polygon()` calls:
        * `bufferSrc.buffer_LinearRing()`
* `fillin()`
    * `fillinSrc.fillin_CoordinateSequence()` (this is the only function that actually does any filling in, as evidenced by it not calling any other `fillinSrc` functions)
        * `calc_dist_between_two_locs()`
        * `great_circle()`
    * `fillinSrc.fillin_LinearRing()` calls:
        * `fillinSrc.fillin_CoordinateSequence()`
    * `fillinSrc.fillin_LineString()` calls:
        * `fillinSrc.fillin_CoordinateSequence()`
    * `fillinSrc.fillin_MultiLineString()` calls:
        * `fillinSrc.fillin_LineString()`
    * `fillinSrc.fillin_MultiPolygon()` calls:
        * `fillinSrc.fillin_Polygon()`
    * `fillinSrc.fillin_Polygon()` calls:
        * `fillinSrc.fillin_LinearRing()`
