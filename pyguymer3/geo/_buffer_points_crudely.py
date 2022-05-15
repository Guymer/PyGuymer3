def _buffer_points_crudely(points1, dist, nang):
    """Buffer some points

    This function reads in an array of coordinates (in degrees) that exist on
    the surface of the Earth and returns an array of coordinates that are the
    rings around the input coordinates buffered by a constant distance (in
    metres).

    Parameters
    ----------
    points1 : numpy.ndarray
            the (npoints, 2) array of (lon,lat) coordinates (in degrees)
    dist : float
            the distance to buffer the (lon,lat) coordinates by (in metres)
    nang : int
            the number of angles around the (lon,lat) coordinates that are calculated when buffering

    Returns
    -------
    points2 : numpy.ndarray
            the (npoints, nang, 2) array of (lon,lat) coordinates around the (lon,lat) coordinates (in degrees)
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist

    # Create short-hand ...
    npoint = points1.shape[0]

    # Initialize array ...
    points2 = numpy.zeros((npoint, nang, 2), dtype = numpy.float64)             # [°]

    # Loop over angles ...
    # NOTE: The first and last angles will *always* be exactly North.
    # NOTE: The most two subsequent points can be apart is ~45 degrees (with
    #       nang >= 9).
    for iang in range(nang):
        # Calculate initial angle ...
        ang1 = 360.0 * float(iang) / float(nang - 1)                            # [°]

        # Loop over points ...
        for ipoint in range(npoint):
            # Calculate the ring coordinates and add them to the array ...
            points2[ipoint, iang, 0], points2[ipoint, iang, 1] = calc_loc_from_loc_and_bearing_and_dist(points1[ipoint, 0], points1[ipoint, 1], ang1, dist)[:2]

    # Return answer ...
    return points2
