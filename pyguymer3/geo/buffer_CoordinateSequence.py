def buffer_CoordinateSequence(coords, dist, kwArgCheck = None, debug = False, fill = -1.0, nang = 19, simp = 0.1):
    """Buffer a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a [Multi]Polygon of the same CoordinateSequence
    buffered by a constant distance (in metres).

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
            the CoordinateSequence
    dist : float
            the distance to buffer each point within the CoordinateSequence by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            how many intermediary points are added to fill in the straight lines which connect the points; negative values disable filling
    nang : int, optional
            the number of angles around each point within the CoordinateSequence that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered CoordinateSequence
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from ._buffer_points import _buffer_points

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None

    # Convert the CoordinateSequence to a NumPy array ...
    raw = numpy.array(coords)                                                   # [°]

    # Check if the user wants to fill in the straight lines which connect the
    # points ...
    if fill > 0.0 and simp > 0.0:
        # Find the Euclidean distance between each raw point ...
        dr = numpy.hypot(numpy.diff(raw[:, 0]), numpy.diff(raw[:, 1]))          # [°]

        # Find the number of filled segments required between each raw point ...
        # NOTE: This is the number of fence panels, not the number of fence
        #       posts.
        ns = (fill * dr / simp).astype(numpy.uint64)                            # [#]
        numpy.place(ns, ns < numpy.uint64(2), numpy.uint64(2))                  # [#]

        # Find the total number of filled points required ...
        # NOTE: This is the total number of fence posts, not the total number of
        #       fence panels.
        nsTot = ns.sum() + numpy.uint64(1)                                      # [#]

        # Create empty array ...
        fill = numpy.zeros((nsTot, 2), dtype = numpy.float64)                   # [°]

        if debug:
            print(f"INFO: There are x{fill.shape[0] / raw.shape[0]:,.1f} more points due to filling.")

        # Initialize index ...
        ifill = numpy.uint64(0)                                                 # [#]

        # Loop over raw points ...
        for iraw in range(ns.size):
            # Fill in points ...
            fill[ifill:ifill + ns[iraw], 0] = numpy.linspace(raw[iraw, 0], raw[iraw + 1, 0], endpoint = False, num = ns[iraw])  # [°]
            fill[ifill:ifill + ns[iraw], 1] = numpy.linspace(raw[iraw, 1], raw[iraw + 1, 1], endpoint = False, num = ns[iraw])  # [°]

            # Increment index ...
            ifill += ns[iraw]                                                   # [#]

        # Fill in last point ...
        fill[-1, :] = raw[-1, :]                                                # [°]

        # Return buffered filled CoordinateSequence ...
        return _buffer_points(fill, dist, debug = debug, nang = nang, simp = simp)

    # Return buffered CoordinateSequence ...
    return _buffer_points(raw, dist, debug = debug, nang = nang, simp = simp)
