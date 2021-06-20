def buffer_MultiPolygon(multipoly, dist, kwArgCheck = None, debug = False, nang = 19, simp = 0.1):
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

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multipoly, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly\" is not a MultiPolygon") from None
    if not multipoly.is_valid:
        raise Exception(f"\"multipoly\" is not a valid MultiPolygon ({shapely.validation.explain_validity(multipoly)})") from None

    # Initialize list ...
    buffs = []

    # Loop over Polygons ...
    for poly in multipoly.geoms:
        # Append buffer of Polygon to list ...
        buffs.append(buffer_Polygon(poly, dist, debug = debug, nang = nang, simp = simp))

    # Convert list of [Multi]Polygons to (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs)

    # Check [Multi]Polygon ...
    if not buffs.is_valid:
        raise Exception(f"\"buffs\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffs)})") from None

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)

        # Check simplified [Multi]Polygon ...
        if buffsSimp.is_valid:
            # Return simplified answer ...
            return buffsSimp

        if debug:
            print(f"WARNING: \"buffsSimp\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffsSimp)}), will return \"buffs\" instead")

    # Return answer ...
    return buffs
