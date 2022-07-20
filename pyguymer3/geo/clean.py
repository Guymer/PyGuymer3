def clean(shape, kwArgCheck = None, debug = False, tol = 1.0e-10):
    """Clean a shape

    This function cleans a shape by removing bad points.

    Parameters
    ----------
    shape : shapely.coords.CoordinateSequence, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the shape
    debug : bool, optional
            print debug messages
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    fills : shapely.coords.CoordinateSequence, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the cleaned shape
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .cleanSrc.clean_CoordinateSequence import clean_CoordinateSequence
    from .cleanSrc.clean_LinearRing import clean_LinearRing
    from .cleanSrc.clean_LineString import clean_LineString
    from .cleanSrc.clean_MultiLineString import clean_MultiLineString
    from .cleanSrc.clean_MultiPolygon import clean_MultiPolygon
    from .cleanSrc.clean_Polygon import clean_Polygon

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check if it is a CoordinateSequence and return it cleaned ...
    if isinstance(shape, shapely.coords.CoordinateSequence):
        return clean_CoordinateSequence(shape, debug = debug, tol = tol)

    # Check if it is a LinearRing and return it cleaned ...
    if isinstance(shape, shapely.geometry.polygon.LinearRing):
        return clean_LinearRing(shape, debug = debug, tol = tol)

    # Check if it is a LineString and return it cleaned ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return clean_LineString(shape, debug = debug, tol = tol)

    # Check if it is a MultiLineString and return it cleaned ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        return clean_MultiLineString(shape, debug = debug, tol = tol)

    # Check if it is a Polygon and return it cleaned ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return clean_Polygon(shape, debug = debug, tol = tol)

    # Check if it is a MultiPolygon and return it cleaned ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return clean_MultiPolygon(shape, debug = debug, tol = tol)

    # Crash ...
    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
