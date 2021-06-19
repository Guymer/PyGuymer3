* A `shapely.geometry.point.Point` is made up of a `shapely.coords.CoordinateSequence` of 1 `tuple` of a (lon,lat) pair.
* A `shapely.geometry.polygon.LinearRing` is made up of a `shapely.coords.CoordinateSequence` of N `tuple`s of (lon,lat) pairs.
* A `shapely.geometry.polygon.Polygon` is made up of a `shapely.geometry.polygon.LinearRing` exterior and N `shapely.geometry.polygon.LinearRing` interiors.
* A `shapely.geometry.multipolygon.MultiPolygon` is made up of N `shapely.geometry.polygon.Polygon`s.
