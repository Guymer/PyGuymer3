def buffer_Polygon(poly, dist, kwArgCheck = None, debug = False, fill = 1.0, nang = 19, simp = 0.1, tol = 1.0e-10):
    """Buffer a Polygon

    This function reads in a Polygon (with an exterior and any number of
    interiors) that exists on the surface of the Earth and returns a
    [Multi]Polygon of the same Polygon buffered by a constant distance (in
    metres).

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
            the Polygon
    dist : float
            the Geodesic distance to buffer each point within the Polygon by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            the Euclidean distance to fill in between each point within the [Multi]Polygon by; negative values disable filling in (in degrees)
    nang : int, optional
            the number of angles around each point within the Polygon that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered Polygon
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._debug import _debug
    from .buffer import buffer
    from .fillin import fillin

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon") from None
    if not poly.is_valid:
        _debug(poly)
        raise Exception(f"\"poly\" is not a valid Polygon ({shapely.validation.explain_validity(poly)})") from None
    if poly.is_empty:
        raise Exception("\"poly\" is an empty Polygon") from None

    # Initialize list ...
    buffs = []
    buffs.append(poly)

    # Append buffer of exterior LinearRing to list ...
    buffs.append(buffer(poly.exterior, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol))

    # Loop over interior LinearRings ...
    for ring in poly.interiors:
        # Append buffer of interior LinearRing to list ...
        buffs.append(buffer(ring, dist, debug = debug, fill = fill, nang = nang, simp = simp, tol = tol))

    # Convert list of [Multi]Polygons to a (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs).simplify(tol)
    if not buffs.is_valid:
        _debug(buffs)
        raise Exception(f"\"buffs\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffs)})") from None
    if buffs.is_empty:
        raise Exception("\"buffs\" is an empty [Multi]Polygon") from None

    # Check if the user wants to fill in the [Multi]Polygon ...
    if fill > 0.0:
        # Fill in [Multi]Polygon ...
        buffs = fillin(buffs, fill, debug = debug, tol = tol)

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)

        # Check simplified [Multi]Polygon ...
        if buffsSimp.is_valid and not buffsSimp.is_empty:
            # Clean up ...
            del buffs

            # Return simplified answer ...
            return buffsSimp

        # Clean up ...
        del buffsSimp

        if debug:
            print(f"WARNING: \"buffsSimp\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffsSimp)}), will return \"buffs\" instead")

    # Return answer ...
    return buffs
