def buffer_LineString(line, dist, kwArgCheck = None, debug = False, fill = 1.0, nang = 19, simp = 0.1, tol = 1.0e-10):
    """Buffer a LineString

    This function reads in a LineString that exists on the surface of the Earth
    and returns a [Multi]Polygon of the same LineString buffered by a constant
    distance (in metres).

    Parameters
    ----------
    line : shapely.geometry.linestring.LineString
            the LineString
    dist : float
            the Geodesic distance to buffer each point within the LineString by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            the Euclidean distance to fill in between each point within the [Multi]Polygon by; negative values disable filling in (in degrees)
    nang : int, optional
            the number of angles around each point within the LineString that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered LineString
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .buffer_CoordinateSequence import buffer_CoordinateSequence

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(line, shapely.geometry.linestring.LineString):
        raise TypeError("\"line\" is not a LineString") from None
    if not line.is_valid:
        raise Exception(f"\"line\" is not a valid LineString ({shapely.validation.explain_validity(line)})") from None
    if line.is_empty:
        raise Exception("\"line\" is an empty LineString") from None

    # Return buffered LineString ...
    return buffer_CoordinateSequence(line.coords, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol)
