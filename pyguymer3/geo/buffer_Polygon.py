def buffer_Polygon(poly, dist, kwArgCheck = None, debug = False, nang = 19, simp = 0.1):
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
            the distance to buffer each point within the Polygon by (in metres)
    debug : bool, optional
            print debug messages
    nang : int, optional
            the number of angles around each point within the Polygon that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)

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

    # Load sub-functions ...
    from .buffer_LinearRing import buffer_LinearRing

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon") from None
    if not poly.is_valid:
        raise Exception(f"\"poly\" is not a valid Polygon ({shapely.validation.explain_validity(poly)})") from None

    # Initialize list ...
    buffs = []
    buffs.append(poly)

    # Buffer exterior LinearRing ...
    buff = buffer_LinearRing(poly.exterior, dist, debug = debug, nang = nang, simp = simp)

    # Check how many Polygons describe the buffer and append them to the list ...
    if isinstance(buff, shapely.geometry.polygon.Polygon):
        if not buff.is_valid:
            raise Exception(f"\"buff\" is not a valid Polygon ({shapely.validation.explain_validity(buff)})") from None
        buffs.append(buff)
    elif isinstance(buff, shapely.geometry.multipolygon.MultiPolygon):
        for geom in buff.geoms:
            if not geom.is_valid:
                raise Exception(f"\"geom\" is not a valid Polygon ({shapely.validation.explain_validity(geom)})") from None
            buffs.append(geom)
    else:
        raise TypeError(f"\"buff\" is an unexpected type {repr(type(buff))}") from None

    # Loop over interior LinearRings ...
    for ring in poly.interiors:
        # Buffer interior LinearRing ...
        buff = buffer_LinearRing(ring, dist, debug = debug, nang = nang, simp = simp)

        # Check how many Polygons describe the buffer and append them to the
        # list ...
        if isinstance(buff, shapely.geometry.polygon.Polygon):
            if not buff.is_valid:
                raise Exception(f"\"buff\" is not a valid Polygon ({shapely.validation.explain_validity(buff)})") from None
            buffs.append(buff)
        elif isinstance(buff, shapely.geometry.multipolygon.MultiPolygon):
            for geom in buff.geoms:
                if not geom.is_valid:
                    raise Exception(f"\"geom\" is not a valid Polygon ({shapely.validation.explain_validity(geom)})") from None
                buffs.append(geom)
        else:
            raise TypeError(f"\"buff\" is an unexpected type {repr(type(buff))}") from None

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
