def fillin(shape, fill, kwArgCheck = None, debug = False, fillSpace = "EuclideanSpace"):
    """Fill in a shape

    This function reads in a shape that exists on the surface of the Earth and
    returns the same shape filled in by a constant distance: either in degrees
    in Euclidean space; or in metres in Geodesic space.

    Parameters
    ----------
    shape : shapely.coords.CoordinateSequence, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the shape
    fill : float
            the Euclidean or Geodesic distance to fill in between each point within the shape by (in degrees or metres)
    debug : bool, optional
            print debug messages
    fillSpace : str, optional
            the geometric space to perform the filling in (either "EuclideanSpace" or "GeodesicSpace")

    Returns
    -------
    fills : shapely.coords.CoordinateSequence, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the filled in shape
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .fillinSrc.fillin_CoordinateSequence import fillin_CoordinateSequence
    from .fillinSrc.fillin_LinearRing import fillin_LinearRing
    from .fillinSrc.fillin_LineString import fillin_LineString
    from .fillinSrc.fillin_MultiLineString import fillin_MultiLineString
    from .fillinSrc.fillin_MultiPolygon import fillin_MultiPolygon
    from .fillinSrc.fillin_Polygon import fillin_Polygon

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check if it is a CoordinateSequence and return it filled ...
    if isinstance(shape, shapely.coords.CoordinateSequence):
        return fillin_CoordinateSequence(shape, fill, debug = debug, fillSpace = fillSpace)

    # Check if it is a LinearRing and return it filled ...
    if isinstance(shape, shapely.geometry.polygon.LinearRing):
        return fillin_LinearRing(shape, fill, debug = debug, fillSpace = fillSpace)

    # Check if it is a LineString and return it filled ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return fillin_LineString(shape, fill, debug = debug, fillSpace = fillSpace)

    # Check if it is a MultiLineString and return it filled ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        return fillin_MultiLineString(shape, fill, debug = debug, fillSpace = fillSpace)

    # Check if it is a Polygon and return it filled ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return fillin_Polygon(shape, fill, debug = debug, fillSpace = fillSpace)

    # Check if it is a MultiPolygon and return it filled ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return fillin_MultiPolygon(shape, fill, debug = debug, fillSpace = fillSpace)

    # Crash ...
    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
