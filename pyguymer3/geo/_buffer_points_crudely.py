def _buffer_points_crudely(points1, dist, nang):
    """Buffer some points

    This function reads in an array of coordinates (in degrees) that exist on
    the surface of the Earth and returns an array of coordinates that are the
    rings around the input coordinates buffered by a constant distance (in
    metres).

    Parameters
    ----------
    points1 : numpy.array
            the (npoints, 2) array of (lon,lat) coordinates (in degrees)
    dist : float
            the distance to buffer the (lon,lat) coordinates by (in metres)
    nang : int
            the number of angles around the (lon,lat) coordinates that are calculated

    Returns
    -------
    points2 : numpy.array
            the (npoints, nang, 2) array of (lon,lat) coordinates around the (lon,lat) coordinates (in degrees)
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Load sub-functions ...
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist

    # Initialize array ...
    points2 = numpy.zeros((points1.shape[0], nang, 2), dtype = numpy.float64)   # [°]

    # Loop over angles ...
    # NOTE: The first and last angles will *always* be exactly North.
    # NOTE: The most two subsequent points can be apart is ~45 degrees (with
    #       nang >= 9).
    for iang in range(nang):
        # Calculate initial angle ...
        ang1 = 360.0 * float(iang) / float(nang - 1)                            # [°]

        # Loop over points ...
        for ipoint in range(points1.shape[0]):
            # Calculate the ring coordinates and add them to the array ...
            points2[ipoint, iang, 0], points2[ipoint, iang, 1], ang2 = calc_loc_from_loc_and_bearing_and_dist(points1[ipoint, 0], points1[ipoint, 1], ang1, dist)

    # Return answer ...
    return points2
