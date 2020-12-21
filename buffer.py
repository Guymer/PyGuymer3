def buffer(shape, dist, nang = 19, simp = 0.1, debug = False):
    """
    This function reads in a shape that exists on the surface of the Earth and
    returns the same shape buffered by a constant distance (in metres).
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

    # Check user input ...
    if not shape.is_valid:
        raise Exception("\"shape\" is not a valid shape ({:s})".format(shapely.validation.explain_validity(shape))) from None

    # Check if it is a Point and return it buffered ...
    if isinstance(shape, shapely.geometry.point.Point):
        return buffer_point(shape.x, shape.y, dist, debug = debug, nang = nang)

    # Check if it is a Polygon and return it buffered ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return buffer_polygon(shape, dist, debug = debug, nang = nang, simp = simp)

    # Check if it is a MultiPolygon and return it buffered ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return buffer_multipolygon(shape, dist, debug = debug, nang = nang, simp = simp)

    # Crash ...
    raise TypeError("\"shape\" is an unexpected type") from None
