def buffer_CoordinateSequence(coords, dist, kwArgCheck = None, debug = False, fill = 1.0, fillSpace = "EuclideanSpace", nang = 19, simp = 0.1, tol = 1.0e-10):
    """Buffer a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a [Multi]Polygon of the same CoordinateSequence
    buffered by a constant distance (in metres).

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
            the CoordinateSequence
    dist : float
            the Geodesic distance to buffer each point within the CoordinateSequence by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            the Euclidean or Geodesic distance to fill in between each point within the shape by (in degrees or metres)
    fillSpace : str, optional
            the geometric space to perform the filling in (either "EuclideanSpace" or "GeodesicSpace")
    nang : int, optional
            the number of angles around each point within the CoordinateSequence that are calculated when buffering
    simp : float, optional
            how much the final [Multi]Polygons is simplified by; negative values disable simplification (in degrees)
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered CoordinateSequence

    Notes
    -----
    According to the Shapely documentation for the method object.buffer():

        "Passed a distance of 0, buffer() can sometimes be used to “clean” self-touching or self-crossing polygons such as the classic “bowtie”. Users have reported that very small distance values sometimes produce cleaner results than 0. Your mileage may vary when cleaning surfaces."

    According to the Shapely documentation for the function shapely.geometry.polygon.orient():

        "A sign of 1.0 means that the coordinates of the product’s exterior ring will be oriented counter-clockwise."
    """

    # Import standard modules ...
    import math
    import multiprocessing

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._buffer_points_crudely import _buffer_points_crudely
    from ._debug import _debug
    from ._points2poly import _points2poly
    from ._posts2panel import _posts2panel
    from .fillin import fillin
    from .remap import remap
    try:
        from ..f90 import funcs
        if debug:
            print("INFO: Will find the rings using FORTRAN.")
        fortran = True
    except:
        if debug:
            print("INFO: Will find the rings using Python.")
        fortran = False

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None

    # Check inputs ...
    if dist < 10.0:
        raise Exception(f"the buffering distance is too small ({dist:,.1f}m < {10.0:,.1f}m)") from None
    if dist > 0.5 * math.pi * 6371008.8:
        raise Exception(f"the buffering distance is too large ({dist:,.1f}m > {0.5 * math.pi * 6371008.8:,.1f}m)") from None
    if nang < 9:
        raise Exception(f"the number of angles is too small ({nang:,d} < {9:,d})") from None

    # **************************************************************************
    # Step 1: Convert the CoordinateSequence to a NumPy array of the original  #
    #         points                                                           #
    # **************************************************************************

    # Check if the user wants to fill in the CoordinateSequence ...
    if fill > 0.0 and len(coords) > 1:
        # Convert the filled in CoordinateSequence to a NumPy array ...
        points1 = numpy.array(fillin(coords, fill, debug = debug, tol = tol).coords)    # [°]
    else:
        # Convert the CoordinateSequence to a NumPy array ...
        points1 = numpy.array(coords)                                           # [°]

    # Create short-hand ...
    npoint = points1.shape[0]

    # Check inputs ...
    if points1[:, 0].min() < -180.0:
        raise Exception(f"a point exists a long way off the W-edge of Earth ({points1[:, 0].min():.6f}° < -180°)") from None
    if points1[:, 0].max() > +180.0:
        raise Exception(f"a point exists a long way off the E-edge of Earth ({points1[:, 0].max():.6f}° > +180°)") from None
    if points1[:, 1].min() < -90.0:
        raise Exception(f"a point exists a long way off the S-edge of Earth ({points1[:, 1].min():.6f}° < -90°)") from None
    if points1[:, 1].max() > +90.0:
        raise Exception(f"a point exists a long way off the N-edge of Earth ({points1[:, 1].max():.6f}° > +90°)") from None

    # **************************************************************************
    # Step 2: Buffer the NumPy array of the original points to get a NumPy     #
    #         array of the rings around them                                   #
    # **************************************************************************

    # Buffer (in Geodesic space) the CoordinateSequence ...
    if fortran:
        points2 = funcs.buffer_points_crudely(points1, dist, nang)              # [°]
    else:
        points2 = _buffer_points_crudely(points1, dist, nang)                   # [°]

    # **************************************************************************
    # Step 3: Convert the NumPy array of the rings around the original points  #
    #         to a list of Polygons of the buffered original points            #
    # **************************************************************************

    # Create a pool of workers ...
    with multiprocessing.Pool() as pool:
        # Initialize list ...
        results = []

        # Loop over points ...
        for ipoint in range(npoint):
            # Add job to pool ...
            results.append(pool.apply_async(_points2poly, (points1[ipoint, :], points2[ipoint, :, :]), {"tol" : tol}))

        # Initialize list ...
        polys = []

        # Loop over results ...
        for result in results:
            # Append (unified) Polygon to list ...
            polys.append(result.get())

        # Clean up ...
        del results

    # **************************************************************************
    # Step 4: Append Polygons of the convex hulls of adjacent buffered         #
    #         original points and the lines that connects them                 #
    # **************************************************************************

    # Initialize list ...
    finalPolys = []

    # Check if there are some connections ...
    if npoint == 1:
        # Append Polygon to list ...
        finalPolys.append(polys[0])
    else:
        # Create a pool of workers ...
        with multiprocessing.Pool() as pool:
            # Initialize list ...
            results = []

            # Loop over points ...
            for ipoint in range(npoint - 1):
                # Add job to pool ...
                results.append(pool.apply_async(_posts2panel, (points1[ipoint, :], points1[ipoint + 1, :], points2[ipoint, :, :], points2[ipoint + 1, :, :], polys[ipoint], polys[ipoint + 1]), {"tol" : tol}))

            # Loop over results ...
            for result in results:
                # Append the convex hull of the two Polygons and the buffered
                # line that connects them to list ...
                finalPolys.append(result.get())

            # Clean up ...
            del results

    # Clean up ...
    del points1, points2, polys

    # **************************************************************************
    # Step 5: Create a single [Multi]Polygon that is the union of all of the   #
    #         Polygons and re-map it so that it does not extend off the edge   #
    #         of Earth                                                         #
    # **************************************************************************

    # Convert list of Polygons to a correctly oriented (unified) Polygon ...
    finalPolys = shapely.geometry.polygon.orient(shapely.ops.unary_union(finalPolys).simplify(tol))
    if not isinstance(finalPolys, shapely.geometry.polygon.Polygon):
        raise Exception("\"finalPolys\" is not a Polygon") from None
    if not finalPolys.is_valid:
        _debug(finalPolys)
        raise Exception(f"\"finalPolys\" is not a valid Polygon ({shapely.validation.explain_validity(finalPolys)})") from None
    if finalPolys.is_empty:
        raise Exception("\"finalPolys\" is an empty Polygon") from None

    # Re-map the Polygon on to Earth ...
    buffs = remap(finalPolys, tol = tol)

    # Clean up ...
    del finalPolys

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
