def fillin_LineString(line, fill, kwArgCheck = None, debug = False, fillSpace = "EuclideanSpace"):
    """Fill in a LineString

    This function reads in a LineString that exists on the surface of the Earth
    and returns a LineString of the same LineString filled in by a constant
    distance: either in degrees in Euclidean space; or in metres in Geodesic
    space.

    Parameters
    ----------
    line : shapely.geometry.linestring.LineString
            the LineString
    fill : float
            the Euclidean or Geodesic distance to fill in between each point within the shape by (in degrees or metres)
    debug : bool, optional
            print debug messages
    fillSpace : str, optional
            the geometric space to perform the filling in (either "EuclideanSpace" or "GeodesicSpace")

    Returns
    -------
    fills : shapely.geometry.linestring.LineString
            the filled in LineString
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
    from .fillin_CoordinateSequence import fillin_CoordinateSequence

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(line, shapely.geometry.linestring.LineString):
        raise TypeError("\"line\" is not a LineString") from None
    if not line.is_valid:
        _debug(line)
        raise Exception(f"\"line\" is not a valid LineString ({shapely.validation.explain_validity(line)})") from None
    if line.is_empty:
        raise Exception("\"line\" is an empty LineString") from None

    # Return filled in LineString ...
    return fillin_CoordinateSequence(line.coords, fill, debug = debug, fillSpace = fillSpace)
