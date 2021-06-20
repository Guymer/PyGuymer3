def buffer_LinearRing(ring, dist, kwArgCheck = None, debug = False, nang = 19, simp = 0.1):
    """Buffer a LinearRing

    This function reads in a LinearRing that exists on the surface of the Earth
    and returns a [Multi]Polygon of the same LinearRing buffered by a constant
    distance (in metres).

    Parameters
    ----------
    ring : shapely.geometry.polygon.LinearRing
            the LinearRing
    dist : float
            the distance to buffer each point within the LinearRing by (in metres)
    debug : bool, optional
            print debug messages
    nang : int, optional
            the number of angles around each point within the LinearRing that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered LinearRing
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from .buffer_CoordinateSequence import buffer_CoordinateSequence

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(ring, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"ring\" is not a LinearRing") from None
    if not ring.is_valid:
        raise Exception(f"\"ring\" is not a valid LinearRing ({shapely.validation.explain_validity(ring)})") from None

    # Buffer CoordinateSequence ...
    buff = buffer_CoordinateSequence(ring.coords, dist, debug = debug, nang = nang, simp = simp)

    # Check [Multi]Polygon ...
    if not buff.is_valid:
        raise Exception(f"\"buff\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buff)})") from None

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffSimp = buff.simplify(simp)

        # Check simplified [Multi]Polygon ...
        if buffSimp.is_valid:
            # Return simplified answer ...
            return buffSimp

        if debug:
            print(f"WARNING: \"buffSimp\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffSimp)}), will return \"buff\" instead")

    # Return answer ...
    return buff
