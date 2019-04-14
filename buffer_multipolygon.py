def buffer_multipolygon(multipoly, dist, nang = 19, simp = 0.1, debug = False):
    """
    This function reads in a MultiPolygon, made up of Polygons (with an exterior
    and any number of interiors), that exists on the surface of the Earth and
    returns the same [Multi]Polygon buffered by a constant distance (in metres).
    """

    # Import modules ...
    import shapely
    import shapely.geometry
    import shapely.ops
    import shapely.validation

    # Load sub-functions ...
    from .buffer_polygon import buffer_polygon

    # Check argument ...
    if not isinstance(multipoly, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly\" is not a MultiPolygon")
    if not multipoly.is_valid:
        raise Exception("\"multipoly\" is not a valid MultiPolygon ({0:s})".format(shapely.validation.explain_validity(multipoly)))

    # Create empty list ...
    buffs = []

    # Loop over Polygons ...
    for poly in multipoly.geoms:
        # Buffer Polygon ...
        buff = buffer_polygon(poly, dist, nang, simp, debug)

        # Check how many polygons describe the buffer and append them to the list ...
        if isinstance(buff, shapely.geometry.multipolygon.MultiPolygon):
            for tmp1 in buff.geoms:
                if not tmp1.is_valid:
                    raise Exception("\"tmp1\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(tmp1)))
                tmp2 = tmp1.simplify(simp)
                if tmp2.is_valid:
                    buffs.append(tmp2)
                else:
                    buffs.append(tmp1)
        elif isinstance(buff, shapely.geometry.polygon.Polygon):
            if not buff.is_valid:
                raise Exception("\"buff\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(buff)))
            tmp1 = buff.simplify(simp)
            if tmp1.is_valid:
                buffs.append(tmp1)
            else:
                buffs.append(buff)
        else:
            raise Exception("\"buff\" is an unexpected type")

    # Convert list to (unified) Polygon and check it ...
    buffs = shapely.ops.unary_union(buffs)
    if not buffs.is_valid:
        raise Exception("\"buffs\" is not a valid [Multi]Polygon ({0:s})".format(shapely.validation.explain_validity(buffs)))

    # Return answer ...
    return buffs
