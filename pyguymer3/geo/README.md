# `pyguymer3.geo`

This is a Python 3.X sub-module containing a bunch of random functions that I have written over the years which are geo-related.

## Documentation

See the following two sources of documentation from GIS software:

* [ArcGIS](https://desktop.arcgis.com/en/arcmap/latest/tools/analysis-toolbox/buffer.htm)
* [QGIS](https://qgis.org/pyqgis/3.22/core/QgsGeometry.html#qgis.core.QgsGeometry.buffer)

## Data Types

* A `shapely.geometry.point.Point` is made up of a `shapely.coords.CoordinateSequence` of 1 `tuple` of a (lon,lat) pair.
* A `shapely.geometry.polygon.LinearRing` is made up of a `shapely.coords.CoordinateSequence` of N `tuple`s of (lon,lat) pairs.
* A `shapely.geometry.polygon.Polygon` is made up of a `shapely.geometry.polygon.LinearRing` exterior and N `shapely.geometry.polygon.LinearRing` interiors.
* A `shapely.geometry.multipolygon.MultiPolygon` is made up of N `shapely.geometry.polygon.Polygon`s.

## Call Graph

Public functions:

* `buffer()`
    * `buffer_CoordinateSequence()`
    * `buffer_Point()`
    * `buffer_LinearRing()`
    * `buffer_Polygon()`
    * `buffer_MultiPolygon()`
* `buffer_CoordinateSequence()` (this is the only function that actually does any buffering)
    * `f90.buffer_points_crudely()` or `_buffer_points_crudely()`
    * `shapely.ops.unary_union()`
    * `shapely.geometry.multipolygon.MultiPolygon.simplify()`
    * `shapely.geometry.polygon.orient()`
    * `shapely.geometry.polygon.Polygon.buffer()`
* `buffer_Point()`
    * `buffer()`
* `buffer_LinearRing()`
    * `buffer()`
* `buffer_Polygon()`
    * `buffer()`
    * `shapely.ops.unary_union()`
    * `shapely.geometry.multipolygon.MultiPolygon.simplify()`
* `buffer_MultiPolygon()`
    * `buffer()`
    * `shapely.ops.unary_union()`
    * `shapely.geometry.multipolygon.MultiPolygon.simplify()`
* `fillin()`
    * `fillin_CoordinateSequence()`
    * `fillin_LinearRing()`
    * `fillin_Polygon()`
    * `fillin_MultiPolygon()`
* `fillin_CoordinateSequence()` (this is the only function that actually does any filling in)
* `fillin_LinearRing()`
    * `fillin()`
* `fillin_Polygon()`
    * `fillin()`
    * `shapely.geometry.polygon.orient()`
* `fillin_MultiPolygon()`
    * `fillin()`
    * `shapely.ops.unary_union()`
