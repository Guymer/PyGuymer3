def buffer(shape, dist, kwArgCheck = None, nang = 19, simp = 0.1, debug = False):
    """Buffer a shape

    This function reads in a shape that exists on the surface of the Earth and
    returns the same shape buffered by a constant distance (in metres).

    Parameters
    ----------
    shape : shapely.geometry.point.Point, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the shape
    dist : float
            the distance to buffer each point within the shape by (in metres)
    nang : int, optional
            the number of angles around each point within the shape that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)
    debug : bool, optional
            print debug messages

    Returns
    -------
    ans : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered shape
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from .buffer_multipolygon import buffer_multipolygon
    from .buffer_point import buffer_point
    from .buffer_polygon import buffer_polygon

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check user input ...
    if not shape.is_valid:
        raise Exception("\"shape\" is not a valid shape ({:s})".format(shapely.validation.explain_validity(shape))) from None

    # Check if it is a Point and return it buffered ...
    if isinstance(shape, shapely.geometry.point.Point):
        return buffer_point(shape.x, shape.y, dist, nang = nang, simp = simp, debug = debug)

    # Check if it is a Polygon and return it buffered ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return buffer_polygon(shape, dist, nang = nang, simp = simp, debug = debug)

    # Check if it is a MultiPolygon and return it buffered ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return buffer_multipolygon(shape, dist, nang = nang, simp = simp, debug = debug)

    # Crash ...
    raise TypeError("\"shape\" is an unexpected type") from None
