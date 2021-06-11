def buffer_multipolygon(multipoly, dist, nang = 19, simp = 0.1, debug = False):
    """Buffer a MultiPolygon

    This function reads in a MultiPolygon, made up of Polygons (with an exterior
    and any number of interiors), that exists on the surface of the Earth and
    returns the same [Multi]Polygon buffered by a constant distance (in metres).

    Parameters
    ----------
    multipoly : shapely.geometry.multipolygon.MultiPolygon
            the MultiPolygon
    dist : float
            the distance to buffer each point within the MultiPolygon by (in metres)
    nang : int, optional
            the number of angles around each point within the MultiPolygon that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)
    debug : bool, optional
            print debug messages

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
    from .buffer_polygon import buffer_polygon

    # Check argument ...
    if not isinstance(multipoly, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly\" is not a MultiPolygon") from None
    if not multipoly.is_valid:
        raise Exception("\"multipoly\" is not a valid MultiPolygon ({0:s})".format(shapely.validation.explain_validity(multipoly))) from None

    # Create empty list ...
    buffs = []

    # Loop over Polygons ...
    for poly in multipoly.geoms:
        # Buffer Polygon ...
        buff = buffer_polygon(poly, dist, nang = nang, simp = simp, debug = debug)

        # Check how many polygons describe the buffer and append them to the
        # list ...
        if isinstance(buff, shapely.geometry.multipolygon.MultiPolygon):
            for geom in buff.geoms:
                if not geom.is_valid:
                    raise Exception("\"geom\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(geom))) from None
                buffs.append(geom)
        elif isinstance(buff, shapely.geometry.polygon.Polygon):
            if not buff.is_valid:
                raise Exception("\"buff\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(buff))) from None
            buffs.append(buff)
        else:
            raise TypeError("\"buff\" is an unexpected type") from None

    # Convert list of Polygons to (unified) MultiPolygon ...
    buffs = shapely.ops.unary_union(buffs)

    # Check MultiPolygon ...
    if not buffs.is_valid:
        raise Exception("\"buffs\" is not a valid [Multi]Polygon ({0:s})".format(shapely.validation.explain_validity(buffs))) from None

    # Check if the user wants to simplify the MultiPolygon ...
    if simp > 0.0:
        # Simplify MultiPolygon ...
        buffsSimp = buffs.simplify(simp)

        # Check simplified MultiPolygon ...
        if buffsSimp.is_valid:
            # Return simplified answer ...
            return buffsSimp

    # Return answer ...
    return buffs
