#!/usr/bin/env python3

# Define function ...
def find_min_max_dist_bearing(
    midLon,
    midLat,
    lons,
    lats,
    /,
    *,
         angConv = 0.1,
    angHalfRange = 180.0,
           debug = False,
            dist = 1000.0,
             eps = 1.0e-12,
           first = True,
           iIter = 0,
            nAng = 9,
           nIter = 100,
           space = "EuclideanSpace",
        startAng = 180.0,
):
    """Find the bearing which points towards the minimum maximum distance to
    some locations

    This function finds the bearing which points towards the minimum maximum
    distance (in either Euclidean space or Geodesic space) to some locations
    around a middle location.

    Parameters
    ----------
    midLon : float
        the middle longitude (in degrees)
    midLat : float
        the middle latitude (in degrees)
    lons : numpy.ndarray
        the longitudes (in degrees)
    lats : numpy.ndarray
        the latitudes (in degrees)
    angConv : float, optional
        the angle change which classifies as converged (in degrees)
    angHalfRange : float, optional
        the angle either side of the starting angle to search over (in degrees)
    debug : bool, optional
        print debug messages
    dist : float, optional
        the distance around the middle location to search over (in degrees or
        metres)
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    first : bool, optional
        flag whether this is the first call of an interative/recursive sequence
        (if the first call then this function just returns the angle with the
        minimum maximum distance; if not the first call then fit a polynomial
        degree 2 to the values and return the calculated root)
    iIter : int, optional
        the current iteration
    nAng : int, optional
        the number of angles around the middle location to search over
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    space : str, optional
        the geometric space to perform the distance calculation in (either
        "EuclideanSpace" or "GeodesicSpace")
    startAng : float, optional
        the starting angle to search over (in degrees)

    Returns
    -------
    rootAng : float
        the angle which points towards the minimum maximum distance to some
        locations (in degrees)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import math

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
    from .max_dist import max_dist

    # **************************************************************************

    # Check arguments ...
    if not isinstance(lons, numpy.ndarray):
        raise TypeError("\"lons\" is not a NumPy array") from None
    if not isinstance(lats, numpy.ndarray):
        raise TypeError("\"lats\" is not a NumPy array") from None
    if iIter == nIter - 1:
        raise Exception(f"failed to converge; the middle is currently ({midLon:.6f}°, {midLat:.6f}°); nIter = {nIter:,d}") from None

    # **************************************************************************

    if debug:
        print(f"INFO: #{iIter + 1:,d}/{nIter:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°) and the minimum maximum distance bearing is now {startAng:.6f}°.")

    # **************************************************************************

    # Make fake angular axis ...
    # NOTE: Taking care not to have the duplicate entries of 0° and 360°.
    if first:
        fakeAngs = numpy.linspace(
            startAng - angHalfRange,
            startAng + angHalfRange,
               dtype = numpy.float64,
            endpoint = False,
                 num = nAng - 1,
        )                                                                       # [°]
    else:
        fakeAngs = numpy.linspace(
            startAng - angHalfRange,
            startAng + angHalfRange,
               dtype = numpy.float64,
            endpoint = True,
                 num = nAng,
        )                                                                       # [°]

    # **************************************************************************

    # Initialize location arrays ...
    angLons = numpy.zeros(
        fakeAngs.size,
        dtype = numpy.float64,
    )                                                                           # [°]
    angLats = numpy.zeros(
        fakeAngs.size,
        dtype = numpy.float64,
    )                                                                           # [°]

    # Populate location arrays ...
    # NOTE: Taking care not to stray outside of 0° and 360°.
    for iAng in range(fakeAngs.size):
        # Check what space the user wants ...
        match space:
            case "EuclideanSpace":
                angLons[iAng] = midLon + dist * math.sin(math.degrees(fakeAngs[iAng]))  # [°]
                angLats[iAng] = midLat + dist * math.cos(math.degrees(fakeAngs[iAng]))  # [°]
            case "GeodesicSpace":
                angLons[iAng], angLats[iAng], _ = calc_loc_from_loc_and_bearing_and_dist(
                    midLon,
                    midLat,
                    (fakeAngs[iAng] + 360.0) % 360.0,
                    dist,
                      eps = eps,
                    nIter = nIter,
                )                                                               # [°], [°]
            case _:
                # Crash ...
                raise ValueError(f"\"space\" is an unexpected value ({repr(space)})") from None

    # **************************************************************************

    # Initialize distance array ...
    maxDists = numpy.zeros(
        fakeAngs.size,
        dtype = numpy.float64,
    )                                                                           # [°] or [m]

    # Populate distance array ...
    for iAng in range(fakeAngs.size):
        maxDists[iAng] = max_dist(
            lons,
            lats,
            angLons[iAng],
            angLats[iAng],
              eps = eps,
            nIter = nIter,
            space = space,
        )                                                                       # [°] or [m]

    # **************************************************************************

    # Check if this is the first time that a solution has been found ...
    if first:
        # Find angle with minimum maximum distance ...
        iAng = maxDists.argmin()                                                # [#]

        # Return answer ...
        return find_min_max_dist_bearing(
            midLon,
            midLat,
            lons,
            lats,
                 angConv = angConv,
            angHalfRange = 0.5 * angHalfRange,
                   debug = debug,
                    dist = dist,
                     eps = eps,
                   first = False,
                   iIter = iIter + 1,
                    nAng = nAng,
                   nIter = nIter,
                   space = space,
                startAng = (fakeAngs[iAng] + 360.0) % 360.0,
        )

    # Fit a polynomial degree 2 to the values and find the best angle ...
    eqn = numpy.polynomial.Polynomial.fit(
        fakeAngs,
        maxDists,
        deg = 2,
    )
    bestAng = eqn.deriv().roots()[0]                                            # [°]

    # Check if the answer is converged ...
    if abs(startAng - bestAng) <= angConv:
        if debug:
            print(f"INFO: #{iIter + 1:,d}/{nIter:,d}: The middle is ({midLon:.6f}°, {midLat:.6f}°) and the minimum maximum distance bearing is {startAng:.6f}°.")
        return bestAng

    # Return answer ...
    return find_min_max_dist_bearing(
        midLon,
        midLat,
        lons,
        lats,
             angConv = angConv,
        angHalfRange = 0.5 * angHalfRange,
               debug = debug,
                dist = dist,
                 eps = eps,
               first = False,
               iIter = iIter + 1,
                nAng = nAng,
               nIter = nIter,
               space = space,
            startAng = (bestAng + 360.0) % 360.0,
    )
