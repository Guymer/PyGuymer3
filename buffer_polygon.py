def buffer_polygon(poly, dist, nang = 19, simp = 0.1, debug = False):
    """
    This function reads in a Polygon (with an exterior and any number of
    interiors) that exists on the surface of the Earth and returns the same
    [Multi]Polygon buffered by a constant distance (in metres).
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

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon")
    if not poly.is_valid:
        raise Exception("\"poly\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(poly))) from None

    # Create pool of workers, create empty lists and append initial Polygon ...
    pool = multiprocessing.Pool()
    results = []
    buff = []
    buff.append(poly)

    # Loop over coordinates in initial Polygon and add buffer job to worker pool...
    for coord in poly.exterior.coords:
        results.append(pool.apply_async(buffer_point, (coord[0], coord[1], dist, nang, debug)))
    for interior in poly.interiors:
        for coord in interior.coords:
            results.append(pool.apply_async(buffer_point, (coord[0], coord[1], dist, nang, debug)))

    # Loop over parallel jobs and append simplified results to list ...
    for result in results:
        buff.append(result.get().simplify(simp))

    # Destroy pool of workers ...
    pool.close()
    pool.join()

    # Convert list to (unified) Polygon and check it ...
    buff = shapely.ops.unary_union(buff)
    if not buff.is_valid:
        raise Exception("\"buff\" is not a valid [Multi]Polygon ({0:s})".format(shapely.validation.explain_validity(buff))) from None

    # Return answer ...
    return buff