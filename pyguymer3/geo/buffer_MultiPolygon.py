def buffer_MultiPolygon(multipoly, dist, kwArgCheck = None, debug = False, fill = 1.0, nang = 19, simp = 0.1):
    """Buffer a MultiPolygon

    This function reads in a MultiPolygon, made up of Polygons (with an exterior
    and any number of interiors), that exists on the surface of the Earth and
    returns a [Multi]Polygon of the same MultiPolygon buffered by a constant
    distance (in metres).

    Parameters
    ----------
    multipoly : shapely.geometry.multipolygon.MultiPolygon
            the MultiPolygon
    dist : float
            the distance to buffer each point within the MultiPolygon by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            the distance to fill in between each point within the [Multi]Polygon by (in degrees)
    nang : int, optional
            the number of angles around each point within the MultiPolygon that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered MultiPolygon
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
    from .buffer_Polygon import buffer_Polygon
    from .fillin import fillin

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multipoly, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly\" is not a MultiPolygon") from None
    if not multipoly.is_valid:
        raise Exception(f"\"multipoly\" is not a valid MultiPolygon ({shapely.validation.explain_validity(multipoly)})") from None
    if multipoly.is_empty:
        raise Exception("\"multipoly\" is an empty MultiPolygon") from None

    # Initialize list ...
    buffs = []

    # Loop over Polygons ...
    for poly in multipoly.geoms:
        # Append buffer of Polygon to list ...
        buffs.append(buffer_Polygon(poly, dist, debug = debug, fill = fill, nang = nang, simp = simp))

    # Convert list of [Multi]Polygons to a correctly oriented (unified)
    # [Multi]Polygon ...
    buffs = shapely.geometry.polygon.orient(shapely.ops.unary_union(buffs).simplify(0))
    if not buffs.is_valid:
        raise Exception(f"\"buffs\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffs)})") from None
    if buffs.is_empty:
        raise Exception("\"buffs\" is an empty [Multi]Polygon") from None

    # Check if the user wants to fill in the [Multi]Polygon ...
    if fill > 0.0:
        # Fill in [Multi]Polygon ...
        buffs = fillin(buffs, fill, debug = debug)

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)

        # Check simplified [Multi]Polygon ...
        if buffsSimp.is_valid and not buffsSimp.is_empty:
            # Return simplified answer ...
            return buffsSimp

        if debug:
            print(f"WARNING: \"buffsSimp\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffsSimp)}), will return \"buffs\" instead")

    # Return answer ...
    return buffs
