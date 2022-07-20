def buffer_Point(point, dist, kwArgCheck = None, debug = False, fill = 1.0, fillSpace = "EuclideanSpace", nang = 19, simp = 0.1, tol = 1.0e-10):
    """Buffer a Point

    This function reads in a Point that exists on the surface of the Earth and
    returns a [Multi]Polygon of the same Point buffered by a constant distance
    (in metres).

    Parameters
    ----------
    point : shapely.geometry.point.Point
            the Point
    dist : float
            the distance to buffer the Point by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            the Euclidean or Geodesic distance to fill in between each point within the shapes by (in degrees or metres)
    fillSpace : str, optional
            the geometric space to perform the filling in (either "EuclideanSpace" or "GeodesicSpace")
    nang : int, optional
            the number of angles around the Point that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered Point
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .._debug import _debug
    from .buffer_CoordinateSequence import buffer_CoordinateSequence

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(point, shapely.geometry.point.Point):
        raise TypeError("\"point\" is not a Point") from None
    if not point.is_valid:
        _debug(point)
        raise Exception(f"\"point\" is not a valid Point ({shapely.validation.explain_validity(point)})") from None
    if point.is_empty:
        raise Exception("\"point\" is an empty Point") from None

    # Return buffered Point ...
    return buffer_CoordinateSequence(point.coords, dist, debug = debug, fill = fill, fillSpace = fillSpace, nang = nang, simp = simp, tol = tol)
