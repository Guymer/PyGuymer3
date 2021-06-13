def buffer_polygon(poly, dist, kwArgCheck = None, nang = 19, simp = 0.1, debug = False):
    """Buffer a Polygon

    This function reads in a Polygon (with an exterior and any number of
    interiors) that exists on the surface of the Earth and returns the same
    [Multi]Polygon buffered by a constant distance (in metres).

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
            the Polygon
    dist : float
            the distance to buffer each point within the Polygon by (in metres)
    nang : int, optional
            the number of angles around each point within the Polygon that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)
    debug : bool, optional
            print debug messages

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered Polygon
    """

    # Import standard modules ...
    import multiprocessing

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from .buffer_point import buffer_point

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon") from None
    if not poly.is_valid:
        raise Exception("\"poly\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(poly))) from None

    # Create pool of workers ...
    with multiprocessing.Pool() as pool:
        # Initialize list ...
        results = []

        # Loop over coordinates in initial Polygon and add buffer job to worker pool...
        for coord in poly.exterior.coords:
            results.append(pool.apply_async(buffer_point, (coord[0], coord[1], dist,), {"nang" : nang, "simp" : simp, "debug" : debug,}))
        for interior in poly.interiors:
            for coord in interior.coords:
                results.append(pool.apply_async(buffer_point, (coord[0], coord[1], dist,), {"nang" : nang, "simp" : simp, "debug" : debug,}))

        # Initialize list ...
        buffs = []
        buffs.append(poly)

        # Loop over parallel jobs and append results to list ...
        for result in results:
            buffs.append(result.get())

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
