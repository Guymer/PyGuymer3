def _buffer_points(points1, dist, kwArgCheck = None, debug = False, nang = 19, simp = 0.1):
    """Buffer some points

    This function reads in an array of coordinates (in degrees) that exist on
    the surface of the Earth and returns a [Multi]Polygon of them buffered by a
    constant distance (in metres).

    Parameters
    ----------
    points1 : numpy.array
            the (npoints, 2) array of (lon,lat) coordinates (in degrees)
    dist : float
            the distance to buffer the (lon,lat) coordinates by (in metres)
    debug : bool, optional
            print debug messages
    nang : int, optional
            the number of angles around the (lon,lat) coordinates that are calculated
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered (lon,lat) coordinates
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
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from ._buffer_points_crudely import _buffer_points_crudely
    from ._fix_ring import _fix_ring
    try:
        from ..f90 import f90
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

    # Check arguments ...
    if not isinstance(points1, numpy.ndarray):
        raise TypeError("\"points1\" is not a numpy.ndarray") from None
    if len(points1.shape) != 2:
        raise TypeError("\"points1\" is not 2D") from None
    if points1.shape[1] != 2:
        raise TypeError("\"points1\" is not (N,2)") from None

    # Correct inputs ...
    dist = max(1.0, min(math.pi * 6371008.8, dist))                             # NOTE: Limit distance to 1m <--> (half-circumference)
    nang = max(9, nang)                                                         # NOTE: Must do at least 9 points around the compass

    # **************************************************************************

    # Buffer the points ...
    if fortran:
        points2 = f90.buffer_points_crudely(points1, dist, nang)                # [°]
    else:
        points2 = _buffer_points_crudely(points1, dist, nang)                   # [°]

    # **************************************************************************

    # Loop over points ...
    for ipoint in range(points1.shape[0]):
        # Check first and last longitude ...
        # if abs(points2[ipoint, 0, 0] - points2[ipoint, -1, 0]) > 0.1:
            # raise Exception(f"the first point in the ring should be much closer to the last point in the ring (machine precision is not this bad): ({points2[ipoint, 0, 0]:f}°,{points2[ipoint, 0, 1]:f}°) versus ({points2[ipoint, -1, 0]:f}°,{points2[ipoint, -1, 1]:f}°). The starting point was #{ipoint:d} at ({points1[ipoint, 0]:f}°,{points1[ipoint, 1]:f}°) and the buffering distance was {dist:f}m.")

        # Check first and last latitude ...
        # if abs(points2[ipoint, 0, 1] - points2[ipoint, -1, 1]) > 0.1:
            # raise Exception(f"the first point in the ring should be much closer to the last point in the ring (machine precision is not this bad): ({points2[ipoint, 0, 0]:f}°,{points2[ipoint, 0, 1]:f}°) versus ({points2[ipoint, -1, 0]:f}°,{points2[ipoint, -1, 1]:f}°). The starting point was #{ipoint:d} at ({points1[ipoint, 0]:f}°,{points1[ipoint, 1]:f}°) and the buffering distance was {dist:f}m.")

        # Replace the last points with the first points ...
        # NOTE: If the first/last point on the ring is very close to longitudes
        #       of ±180.0° and/or latitudes of ±90.0° then the iterative
        #       function can return coordinates which are hundreds of degrees
        #       different but which are only mm apart. For example:
        #         * (-180.0°, +50.0°) is exactly the same as (+180.0°, +50.0°)
        #         * (-50.0°, +90.0°) is exactly the same as (+50.0°, +90.0°)
        #       This can cause havoc with Shapely when constructing LinearRings
        #       from these NumPy arrays. To side-step this issue I arbitrarily
        #       replace the last point (which was calculated using an angle of
        #       360°) with the first point (which was calculated using an angle
        #       of 0°).
        points2[ipoint, -1, :] = points2[ipoint, 0, :]                          # [°]

    # **************************************************************************

    # Initialize list ...
    buffs = []

    # Create pool of workers ...
    with multiprocessing.Pool() as pool:
        # Initialize list ...
        results = []

        # Loop over points ...
        for ipoint in range(points1.shape[0]):
            # Add an asynchronous parallel job to the pool to convert the array
            # of coordinates to a [Multi]Polygon ...
            results.append(pool.apply_async(_fix_ring, (points2[ipoint, :, :],), {"debug" : debug, "simp" : simp}))

        # Loop over parallel jobs and append results to list ...
        for result in results:
            buffs.append(result.get())

    # **************************************************************************

    # Convert list of [Multi]Polygons to (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs)

    # Check [Multi]Polygon ...
    if not buffs.is_valid:
        raise Exception(f"\"buffs\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffs)})") from None

    # Check [Multi]Polygon ...
    if buffs.is_empty:
        raise Exception("\"buffs\" is an empty [Multi]Polygon") from None

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
