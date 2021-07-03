def buffer_Point(point, dist, kwArgCheck = None, debug = False, nang = 19, simp = 0.1):
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
    nang : int, optional
            the number of angles around the Point that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)

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

    # Load sub-functions ...
    from .buffer_CoordinateSequence import buffer_CoordinateSequence

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(point, shapely.geometry.point.Point):
        raise TypeError("\"point\" is not a Point") from None
    if not point.is_valid:
        raise Exception(f"\"point\" is not a valid Point ({shapely.validation.explain_validity(point)})") from None
    if point.is_empty:
        raise Exception("\"point\" is an empty Point") from None

    # Return buffered Point ...
    return buffer_CoordinateSequence(point.coords, dist, debug = debug, nang = nang, simp = simp)
