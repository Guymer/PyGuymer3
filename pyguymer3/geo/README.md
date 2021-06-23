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
* `buffer_CoordinateSequence()`
    * `_buffer_points()` (this is the only function that actually does any buffering)
* `buffer_Point()`
    * `buffer_CoordinateSequence()`
* `buffer_LinearRing()`
    * `buffer_CoordinateSequence()`
* `buffer_Polygon()`
    * `buffer_LinearRing()`
    * `shapely.ops.unary_union()`
    * `shapely.geometry.multipolygon.MultiPolygon.simplify()`
* `buffer_MultiPolygon()`
    * `buffer_Polygon()`
    * `shapely.ops.unary_union()`
    * `shapely.geometry.multipolygon.MultiPolygon.simplify()`

Private functions:

* `_buffer_points()` (returns a [Multi]Polygon made up of the simplified union of N [Multi]Polygons)
    * `f90.buffer_points_crudely()` or `_buffer_points_crudely()`
    * `_fix_ring()`
    * `shapely.ops.unary_union()`
    * `shapely.geometry.multipolygon.MultiPolygon.simplify()`
* `_fix_ring()` (returns a [Multi]Polygon made up of the simplified union of 1 or 2 Polygons)
    * `shapely.ops.unary_union()`
    * `shapely.geometry.multipolygon.MultiPolygon.simplify()`
