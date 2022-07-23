def _buffer_points_crudely(points1, dist, nang, kwArgCheck = None, debug = False, tol = 1.0e-10):
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
            the number of angles around the (lon,lat) coordinates that are calculated when buffering (must be odd; must be ≥ 9)
    debug : bool, optional
            print debug messages
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    points2 : numpy.ndarray
            the (npoint, nang, 2) array of (lon,lat) coordinates around the (lon,lat) coordinates (in degrees)
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Create short-hand ...
    npoint = points1.shape[0]

    # Initialize array ...
    points2 = numpy.zeros((npoint, nang, 2), dtype = numpy.float64)             # [°]

    # Loop over angles ...
    # NOTE: The first and last angles will *always* be exactly North (therefore,
    #       use that as a check later on).
    # NOTE: The middle angle will *always* be exactly South (therefore, use that
    #       as a check later on).
    # NOTE: The most two subsequent points can be apart is ~45° (with nang ≥ 9).
    for iang in range(nang):
        # Calculate initial angle ...
        ang1 = 360.0 * float(iang) / float(nang - 1)                            # [°]

        # Loop over points ...
        for ipoint in range(npoint):
            # Calculate the ring coordinates and add them to the array ...
            points2[ipoint, iang, 0], points2[ipoint, iang, 1] = calc_loc_from_loc_and_bearing_and_dist(points1[ipoint, 0], points1[ipoint, 1], ang1, dist)[:2]

    # Loop over points ...
    for ipoint in range(npoint):
        # Create short-hands ...
        dlon1 = abs(points1[ipoint, 0] - points2[ipoint, 0, 0])                 # [°]
        dlon2 = abs(points1[ipoint, 0] - points2[ipoint, nang - 1, 0])          # [°]
        if (dlon1 < tol) != (dlon2 < tol):
            raise Exception("the first/last points are inconsistent") from None

        # Check if it goes over the North Pole ...
        if dlon1 >= tol:
            if debug:
                print("INFO: The two 12 o'clock positions are not vertically inline with the central point.")

            # Initialize array ...
            points3 = numpy.zeros((nang, 2), dtype = numpy.float64)             # [°]

            # Fix the first point ...
            points3[0, 0] = points2[ipoint, 0, 0]                               # [°]
            points3[0, 1] = 90.0                                                # [°]

            # Fix the last point ...
            points3[nang - 1, 0] = points2[ipoint, nang - 1, 0]                 # [°]
            points3[nang - 1, 1] = 90.0                                         # [°]

            # Fill in the first-half and the second-half of the points (missing
            # out 3 o'clock and 9 o'clock to make space for the two new points) ...
            iang3 = 1
            for iang2 in range(nang):
                if iang2 in [(nang - 1) // 4, (3 * (nang - 1)) // 4,]:
                    continue
                points3[iang3, 0] = points2[ipoint, iang2, 0]                   # [°]
                points3[iang3, 1] = points2[ipoint, iang2, 1]                   # [°]
                iang3 += 1

            # Overwrite the points and skip ...
            points2[ipoint, :, :] = points3[:, :]                               # [°]
            continue

        # Create short-hands ...
        dlon3 = abs(points1[ipoint, 0] - points2[ipoint, (nang - 1) // 2, 0])   # [°]

        # Check if it goes over the South Pole ...
        if dlon3 >= tol:
            if debug:
                print("INFO: The 6 o'clock position is not vertically inline with the central point.")

            # Initialize array ...
            points3 = numpy.zeros((nang, 2), dtype = numpy.float64)             # [°]

            # Fill in the first-half of the points (missing out 3 o'clock to
            # make space for the three new points) ...
            iang3 = 0
            for iang2 in range((nang - 1) // 2):
                if iang2 == (nang - 1) // 4:
                    continue
                points3[iang3, 0] = points2[ipoint, iang2, 0]                   # [°]
                points3[iang3, 1] = points2[ipoint, iang2, 1]                   # [°]
                iang3 += 1

            # Replace the single point with a pair of points 0.05° either side
            # of it (this is required because the point is not duplicated,
            # unlike at the North Pole) ...
            endLon1, endLat1 = calc_loc_from_loc_and_bearing_and_dist(points1[ipoint, 0], points1[ipoint, 1], 179.95, dist)[:2]
            endLon2, endLat2 = calc_loc_from_loc_and_bearing_and_dist(points1[ipoint, 0], points1[ipoint, 1], 180.05, dist)[:2]

            # Add the point before it crosses the South Pole ...
            points3[iang3, 0] = endLon1                                         # [°]
            points3[iang3, 1] = endLat1                                         # [°]
            iang3 += 1

            # Fix the point before it crosses the South Pole ...
            points3[iang3, 0] = endLon1                                         # [°]
            points3[iang3, 1] = -90.0                                           # [°]
            iang3 += 1

            # Fix the point after it crosses the South Pole ...
            points3[iang3, 0] = endLon2                                         # [°]
            points3[iang3, 1] = -90.0                                           # [°]
            iang3 += 1

            # Add the point after it crosses the South Pole ...
            points3[iang3, 0] = endLon2                                         # [°]
            points3[iang3, 1] = endLat2                                         # [°]
            iang3 += 1

            # Fill in the second-half of the points (missing out 9 o'clock and
            # the second 12 o'clock to make space for the three new points) ...
            for iang2 in range((nang - 1) // 2 + 1, nang - 1):
                if iang2 == (3 * (nang - 1)) // 4:
                    continue
                points3[iang3, 0] = points2[ipoint, iang2, 0]                   # [°]
                points3[iang3, 1] = points2[ipoint, iang2, 1]                   # [°]
                iang3 += 1

            # Overwrite the points and skip ...
            points2[ipoint, :, :] = points3[:, :]                               # [°]
            continue

    # Return answer ...
    return points2
