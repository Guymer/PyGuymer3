def fillin_CoordinateSequence(coords, fill, kwArgCheck = None, debug = False):
    """Fill in a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a LineString of the same CoordinateSequence filled in
    by a constant distance (in degrees).

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
            the CoordinateSequence
    fill : float
            the distance to fill in between each point within the shape by (in degrees)
    debug : bool, optional
            print debug messages

    Returns
    -------
    fills : shapely.geometry.linestring.LineString
            the filled in CoordinateSequence
    """

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

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None

    # Convert the CoordinateSequence to a NumPy array ...
    points1 = numpy.array(coords)                                               # [°]

    # Find the Euclidean distance between each original point ...
    dr = numpy.hypot(numpy.diff(points1[:, 0]), numpy.diff(points1[:, 1]))      # [°]

    # Find the number of filled segments required between each original point ...
    # NOTE: This is the number of fence panels, not the number of fence posts.
    ns = (dr / fill).astype(numpy.uint64)                                       # [#]
    numpy.place(ns, ns < numpy.uint64(2), numpy.uint64(2))                      # [#]

    # Find the total number of filled points required ...
    # NOTE: This is the total number of fence posts, not the total number of
    #       fence panels.
    nsTot = ns.sum() + numpy.uint64(1)                                          # [#]

    # Create empty array ...
    points2 = numpy.zeros((nsTot, 2), dtype = numpy.float64)                    # [°]

    if debug:
        print(f"INFO: There are x{points2.shape[0] / points1.shape[0]:,.1f} more points due to filling.")

    # Initialize index ...
    ifill = numpy.uint64(0)                                                     # [#]

    # Loop over original points ...
    for ipoint in range(ns.size):
        # Fill in points ...
        points2[ifill:ifill + ns[ipoint], 0] = numpy.linspace(
            points1[ipoint, 0],
            points1[ipoint + 1, 0],
            endpoint = False,
            num = ns[ipoint]
        )                                                                       # [°]
        points2[ifill:ifill + ns[ipoint], 1] = numpy.linspace(
            points1[ipoint, 1],
            points1[ipoint + 1, 1],
            endpoint = False,
            num = ns[ipoint]
        )                                                                       # [°]

        # Increment index ...
        ifill += ns[ipoint]                                                     # [#]

    # Fill in last point ...
    points2[-1, :] = points1[-1, :]                                             # [°]

    # Convert array of points to a LineString ...
    fills = shapely.geometry.linestring.LineString(points2)
    if not isinstance(fills, shapely.geometry.linestring.LineString):
        raise TypeError("\"fills\" is not a LineString") from None
    if not fills.is_valid:
        raise Exception(f"\"fills\" is not a valid LineString ({shapely.validation.explain_validity(fills)})") from None
    if fills.is_empty:
        raise Exception("\"fills\" is an empty LineString") from None

    # Return answer ...
    return fills
