def buffer(shape, dist, kwArgCheck = None, debug = False, fill = 1.0, nang = 19, simp = 0.1, tol = 1.0e-10):
    """Buffer a shape

    This function reads in a shape that exists on the surface of the Earth and
    returns the same shape buffered by a constant distance (in metres).

    Parameters
    ----------
    shape : shapely.coords.CoordinateSequence, shapely.geometry.point.Point, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the shape
    dist : float
            the Geodesic distance to buffer each point within the shape by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            the Euclidean distance to fill in between each point within the shape by; negative values disable filling in (in degrees)
    nang : int, optional
            the number of angles around each point within the shape that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered shape
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .buffer_CoordinateSequence import buffer_CoordinateSequence
    from .buffer_LinearRing import buffer_LinearRing
    from .buffer_LineString import buffer_LineString
    from .buffer_MultiLineString import buffer_MultiLineString
    from .buffer_MultiPolygon import buffer_MultiPolygon
    from .buffer_Point import buffer_Point
    from .buffer_Polygon import buffer_Polygon

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check if it is a CoordinateSequence and return it buffered ...
    if isinstance(shape, shapely.coords.CoordinateSequence):
        return buffer_CoordinateSequence(shape, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol)

    # Check if it is a Point and return it buffered ...
    if isinstance(shape, shapely.geometry.point.Point):
        return buffer_Point(shape, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol)

    # Check if it is a LinearRing and return it buffered ...
    if isinstance(shape, shapely.geometry.polygon.LinearRing):
        return buffer_LinearRing(shape, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol)

    # Check if it is a LineString and return it buffered ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return buffer_LineString(shape, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol)

    # Check if it is a MultiLineString and return it buffered ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        return buffer_MultiLineString(shape, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol)

    # Check if it is a Polygon and return it buffered ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return buffer_Polygon(shape, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol)

    # Check if it is a MultiPolygon and return it buffered ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return buffer_MultiPolygon(shape, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol)

    # Crash ...
    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
