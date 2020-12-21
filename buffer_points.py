def buffer_points(points, dist, nang = 19, simp = 0.1, debug = False):
    """
    This function reads in a list of (lon,lat)-tuples representing coordinates
    (in degrees) that exist on the surface of the Earth and returns a
    [Multi]Polygon of them buffered by a constant distance (in metres).
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
        results.append(pool.apply_async(buffer_point, (point[0], point[1], dist, nang, debug)))

    # Loop over parallel jobs and append simplified results to list ...
    for result in results:
        buffs.append(result.get().simplify(simp))

    # Destroy pool of workers ...
    pool.close()
    pool.join()

    # Convert list to (unified) Polygon and check it ...
    buffs = shapely.ops.unary_union(buffs)
    if not buffs.is_valid:
        raise Exception("\"buffs\" is not a valid [Multi]Polygon ({0:s})".format(shapely.validation.explain_validity(buffs))) from None

    # Return answer ...
    return buffs
