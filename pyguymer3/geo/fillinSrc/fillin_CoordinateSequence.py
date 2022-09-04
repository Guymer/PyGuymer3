def fillin_CoordinateSequence(coords, fill, kwArgCheck = None, fillSpace = "EuclideanSpace", debug = False):
    """Fill in a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a LineString of the same CoordinateSequence filled in
    by a constant distance: either in degrees in Euclidean space; or in metres
    in Geodesic space.

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
        the CoordinateSequence
    fill : float
        the Euclidean or Geodesic distance to fill in between each point within
        the shape by (in degrees or metres)
    debug : bool, optional
        print debug messages
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")

    Returns
    -------
    fills : shapely.geometry.linestring.LineString
        the filled in CoordinateSequence

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ..calc_dist_between_two_locs import calc_dist_between_two_locs
    from ..check import check
    from ..great_circle import great_circle

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None
    check(coords)

    # Convert the CoordinateSequence to a NumPy array ...
    points1 = numpy.array(coords)                                               # [°]

    # Check what space the user wants to fill in ...
    if fillSpace == "EuclideanSpace":
        # Find the Euclidean distance between each original point ...
        dr = numpy.hypot(numpy.diff(points1[:, 0]), numpy.diff(points1[:, 1]))  # [°]
    elif fillSpace == "GeodesicSpace":
        # Find the Geodesic distance between each original point ...
        dr = numpy.zeros(points1.shape[0] - 1, dtype = numpy.float64)           # [m]
        for ipoint in range(dr.size):
            dr[ipoint], _alpha1, _alpha1 = calc_dist_between_two_locs(
                points1[ipoint    , 0],
                points1[ipoint    , 1],
                points1[ipoint + 1, 0],
                points1[ipoint + 1, 1],
            )                                                                   # [m], [°], [°]
    else:
        raise Exception(f"unrecognised value of \"fillSpace\" ({fillSpace})") from None

    # Find the number of filled segments required between each original point ...
    # NOTE: This is the number of fence panels, not the number of fence posts.
    ns = (dr / fill).astype(numpy.uint64)                                       # [#]
    numpy.place(ns, ns < numpy.uint64(1), numpy.uint64(1))                      # [#]

    # Clean up ...
    del dr

    # Find the total number of filled points required ...
    # NOTE: This is the total number of fence posts, not the total number of
    #       fence panels.
    nsTot = ns.sum() + numpy.uint64(1)                                          # [#]

    # Create empty array ...
    points2 = numpy.zeros((nsTot, 2), dtype = numpy.float64)                    # [°]

    if debug:
        print(f"INFO: There are x{points2.shape[0] / points1.shape[0]:,.1f} more points due to filling in.")

    # Initialize index ...
    ifill = numpy.uint64(0)                                                     # [#]

    # Check what space the user wants to fill in ...
    if fillSpace == "EuclideanSpace":
        # Loop over original points ...
        for ipoint in range(ns.size):
            # Fill in points ...
            points2[ifill:ifill + ns[ipoint], 0] = numpy.linspace(
                points1[ipoint, 0],
                points1[ipoint + 1, 0],
                endpoint = False,
                     num = ns[ipoint],
            )                                                                   # [°]
            points2[ifill:ifill + ns[ipoint], 1] = numpy.linspace(
                points1[ipoint, 1],
                points1[ipoint + 1, 1],
                endpoint = False,
                     num = ns[ipoint],
            )                                                                   # [°]

            # Increment index ...
            ifill += ns[ipoint]                                                 # [#]
    elif fillSpace == "GeodesicSpace":
        # Loop over original points ...
        for ipoint in range(ns.size):
            # Find the great circle connecting the two original points and
            # convert the LineString to a NumPy array ...
            arc = great_circle(
                points1[ipoint    , 0],
                points1[ipoint    , 1],
                points1[ipoint + 1, 0],
                points1[ipoint + 1, 1],
                 debug = debug,
                npoint = ns[ipoint] + numpy.uint64(1),
            )
            arcCoords = numpy.array(arc.coords)                                 # [°]

            # Clean up ...
            del arc

            # Fill in points ...
            points2[ifill:ifill + ns[ipoint], 0] = arcCoords[:-1, 0]            # [°]
            points2[ifill:ifill + ns[ipoint], 1] = arcCoords[:-1, 1]            # [°]

            # Clean up ...
            del arcCoords

            # Increment index ...
            ifill += ns[ipoint]                                                 # [#]
    else:
        raise Exception(f"unrecognised value of \"fillSpace\" ({fillSpace})") from None

    # Clean up ...
    del ns

    # Fill in last point ...
    points2[-1, :] = points1[-1, :]                                             # [°]

    # Clean up ...
    del points1

    # Convert array of points to a LineString ...
    fills = shapely.geometry.linestring.LineString(points2)
    check(fills)

    # Clean up ...
    del points2

    # Return answer ...
    return fills
