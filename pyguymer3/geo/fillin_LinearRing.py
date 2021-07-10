def fillin_LinearRing(ring, fill, kwArgCheck = None, debug = False):
    """Fill in a LinearRing

    This function reads in a LinearRing that exists on the surface of the Earth
    and returns a LinearRing of the same LinearRing filled in by a constant
    distance (in degrees).

    Parameters
    ----------
    ring : shapely.geometry.polygon.LinearRing
            the LinearRing
    fill : float
            the distance to fill in between each point within the shape by (in degrees)
    debug : bool, optional
            print debug messages

    Returns
    -------
    fills : shapely.geometry.linestring.LineString
            the filled in LinearRing
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from .fillin_CoordinateSequence import fillin_CoordinateSequence

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(ring, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"ring\" is not a LinearRing") from None
    if not ring.is_valid:
        raise Exception(f"\"ring\" is not a valid LinearRing ({shapely.validation.explain_validity(ring)})") from None
    if ring.is_empty:
        raise Exception("\"ring\" is an empty LinearRing") from None

    # Return filled in LinearRing ...
    return fillin_CoordinateSequence(ring.coords, fill, debug = debug)
