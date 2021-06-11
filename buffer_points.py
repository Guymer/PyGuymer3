def buffer_points(points, dist, nang = 19, simp = 0.1, debug = False):
    """Buffer some points

    This function reads in a list of (lon,lat)-tuples representing coordinates
    (in degrees) that exist on the surface of the Earth and returns a
    [Multi]Polygon of them buffered by a constant distance (in metres).

    Parameters
    ----------
    points : list of tuples of floats
            the (lon,lat)-tuples of the points
    dist : float
            the distance to buffer the points by
    nang : int, optional
            the number of angles around each point that are calculated
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by (in degrees)
    debug : bool, optional
            print debug messages

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered points
    """

    # Import standard modules ...
    import multiprocessing

    # Import special modules ...
    try:
        import shapely
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from .buffer_point import buffer_point

    # Check argument ...
    if not isinstance(points, list):
        raise TypeError("\"points\" is not a list") from None

    # Create pool of workers and create empty lists ...
    pool = multiprocessing.Pool()
    results = []
    buffs = []

    # Loop over points in points list and add buffer job to worker pool...
    for point in points:
        results.append(pool.apply_async(buffer_point, (point[0], point[1], dist,), {"nang" : nang, "simp" : simp, "debug" : debug,}))

    # Loop over parallel jobs and append results to list ...
    for result in results:
        buffs.append(result.get())

    # Destroy pool of workers ...
    pool.close()
    pool.join()

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
