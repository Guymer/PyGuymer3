#!/usr/bin/env python3

# Define function ...
def _buffer_points_crudely(points1, dist, nang, /, *, eps = 1.0e-12, nmax = 100, ramLimit = 1073741824):
    """Buffer some points

    This function reads in an array of coordinates (in degrees) that exist on
    the surface of the Earth and returns an array of coordinates that are the
    rings around the input coordinates buffered by a constant distance (in
    metres).

    Parameters
    ----------
    points1 : numpy.ndarray
        the (npoint, 2) array of (lon,lat) coordinates (in degrees)
    dist : float
        the distance to buffer the (lon,lat) coordinates by (in metres)
    nang : int
        the number of angles around the (lon,lat) coordinates that are
        calculated when buffering (must be odd; must be ≥ 9)
    ramLimit : int, optional
        the maximum RAM usage of each "large" array, in bytes

    Returns
    -------
    points2 : numpy.ndarray
        the (npoint, nang, 2) array of (lon,lat) coordinates around the
        (lon,lat) coordinates (in degrees)

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

    # Import sub-functions ...
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist

    # Create short-hand ...
    npoint = points1.shape[0]

    # Check array size ...
    if npoint * nang * 2 * 8 > ramLimit:
        raise Exception(f"\"points2\" is going to be {npoint * nang * 2 * 8:,d} bytes, which is larger than {ramLimit:,d} bytes") from None

    # Initialize array ...
    points2 = numpy.zeros((npoint, nang, 2), dtype = numpy.float64)             # [°]

    # Loop over angles ...
    # NOTE: The first and last angles will *always* be exactly North (therefore,
    #       use that as a check later on).
    # NOTE: The middle angle will *always* be exactly South (therefore, use that
    #       as a check later on).
    # NOTE: The most two subsequent points can be apart is ~45° (with nang ≥ 9).
    for iang in range(nang - 1):
        # Calculate initial angle ...
        ang1 = 360.0 * float(nang - 1 - iang) / float(nang - 1)                 # [°]

        # Loop over points ...
        for ipoint in range(npoint):
            # Calculate the ring coordinates and add them to the array ...
            points2[ipoint, iang, 0], points2[ipoint, iang, 1], _ = calc_loc_from_loc_and_bearing_and_dist(
                points1[ipoint, 0],
                points1[ipoint, 1],
                ang1 % 360.0,
                dist,
                 eps = eps,
                nmax = nmax,
            )                                                                   # [°], [°]

    # Loop over points ...
    for ipoint in range(npoint):
        # Force the last point to be the same as the first point ...
        points2[ipoint, -1, :] = points2[ipoint, 0, :]                          # [°]

    # Return answer ...
    return points2
