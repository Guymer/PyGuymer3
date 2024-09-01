#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs_geodesicCircle(
    lons,
    lats,
    /,
    *,
     conv = 1.0e3,
    debug = False,
      eps = 1.0e-12,
    nIter = 10,
     nMax = 100,
      pad = 10.0e3,
):
    """Find the middle of some locations such that they are encompassed by the
    smallest Geodesic circle possible.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .find_middle_of_locs_euclideanBox import find_middle_of_locs_euclideanBox
    from ..calc_dist_between_two_locs import calc_dist_between_two_locs
    from ..calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
    from ..max_dist import max_dist

    # **************************************************************************

    # Check arguments ...
    if not isinstance(lons, numpy.ndarray):
        raise TypeError("\"lons\" is not a NumPy array") from None
    if not isinstance(lats, numpy.ndarray):
        raise TypeError("\"lats\" is not a NumPy array") from None

    # **************************************************************************

    # Calculate the middle of the Euclidean bounding box to use as a decent
    # starting guess ...
    midLon, midLat, _ = find_middle_of_locs_euclideanBox(
        lons,
        lats,
        debug = debug,
          eps = eps,
         nMax = nMax,
          pad = -1.0,
    )                                                                           # [°], [°]

    if debug:
        print(f"INFO: The initial middle is ({midLon:.6f}°, {midLat:.6f}°).")

    # Loop over iterations ...
    for iIter in range(nIter):
        # Create short-hand ...
        pntC = (midLon, midLat)                                                 # [°], [°]

        # Find points North/South/East/West of the central point ...
        pntN = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            0.0,
            conv,
             eps = eps,
            nMax = nMax,
        )[:2]                                                                   # [°], [°]
        pntNN = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            0.0,
            2.0 * conv,
             eps = eps,
            nMax = nMax,
        )[:2]                                                                   # [°], [°]
        pntE = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            90.0,
            conv,
             eps = eps,
            nMax = nMax,
        )[:2]                                                                   # [°], [°]
        pntEE = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            90.0,
            2.0 * conv,
             eps = eps,
            nMax = nMax,
        )[:2]                                                                   # [°], [°]
        pntS = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            180.0,
            conv,
             eps = eps,
            nMax = nMax,
        )[:2]                                                                   # [°], [°]
        pntSS = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            180.0,
            2.0 * conv,
             eps = eps,
            nMax = nMax,
        )[:2]                                                                   # [°], [°]
        pntW = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            270.0,
            conv,
             eps = eps,
            nMax = nMax,
        )[:2]                                                                   # [°], [°]
        pntWW = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            270.0,
            2.0 * conv,
             eps = eps,
            nMax = nMax,
        )[:2]                                                               # [°], [°]

        # Find the maximum Geodesic distance from the points to any location ...
        distC = max_dist(
            lons,
            lats,
            pntC[0],
            pntC[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]
        distN = max_dist(
            lons,
            lats,
            pntN[0],
            pntN[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]
        distNN = max_dist(
            lons,
            lats,
            pntNN[0],
            pntNN[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]
        distE = max_dist(
            lons,
            lats,
            pntE[0],
            pntE[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]
        distEE = max_dist(
            lons,
            lats,
            pntEE[0],
            pntEE[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]
        distS = max_dist(
            lons,
            lats,
            pntS[0],
            pntS[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]
        distSS = max_dist(
            lons,
            lats,
            pntSS[0],
            pntSS[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]
        distW = max_dist(
            lons,
            lats,
            pntW[0],
            pntW[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]
        distWW = max_dist(
            lons,
            lats,
            pntWW[0],
            pntWW[1],
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]

        # Fit Polynomials (degree 2) to the South/North line and the West/East
        # line ...
        eqnSN = numpy.polynomial.Polynomial.fit(
            [
                pntSS[1],
                pntS[1],
                pntC[1],
                pntN[1],
                pntNN[1],
            ],
            [
                distSS,
                distS,
                distC,
                distN,
                distNN,
            ],
               deg = 2,
            domain = [-90.0, +90.0],
            symbol = "y",
        )
        eqnWE = numpy.polynomial.Polynomial.fit(
            [
                pntWW[0],
                pntW[0],
                pntC[0],
                pntE[0],
                pntEE[0],
            ],
            [
                distWW,
                distW,
                distC,
                distE,
                distEE,
            ],
               deg = 2,
            domain = [-180.0, +180.0],
            symbol = "x",
        )

        # Find the roots of the two Polynomials and use them to guess where the
        # local minimum is ...
        pntGuess = (eqnWE.deriv().roots()[0], eqnSN.deriv().roots()[0])         # [°], [°]

        # Calculate the Geodesic distance and Geodesic bearing to the guess of
        # the local minimum ...
        dist, bear, _ = calc_dist_between_two_locs(
            pntC[0],
            pntC[1],
            pntGuess[0],
            pntGuess[1],
             eps = eps,
            nMax = nMax,
        )                                                                       # [m], [°]

        # Check if we can stop iterating ...
        if dist < conv:
            break

        # Stop if the end of the loop has been reached but the answer has not
        # converged ...
        if iIter == nIter - 1:
            raise Exception(f"failed to converge; the middle is currently ({midLon:.6f}°, {midLat:.6f}°); nIter = {nIter:d}") from None

        if debug:
            print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * conv:,.1f} km towards {bear:.1f}° ...")

        # Update the middle location ...
        midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
            pntC[0],
            pntC[1],
            bear,
            conv,
             eps = eps,
            nMax = nMax,
        )                                                                       # [°], [°]

        if debug:
            print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")

    # Find the maximum Geodesic distance from the middle to any location ...
    maxDist = max_dist(
        lons,
        lats,
        midLon,
        midLat,
          eps = eps,
         nMax = nMax,
        space = "GeodesicSpace",
    )                                                                           # [m]

    if debug:
        print(f"INFO: Maximum Geodesic distance is {0.001 * maxDist:,.1f} km.")

    # **************************************************************************

    # Check if a padding needs to be added ...
    if pad > 0.0:
        # Add padding ...
        maxDist += pad                                                          # [m]

        if debug:
            print(f"INFO: Maximum (padded) Geodesic distance is {0.001 * maxDist:,.1f} km.")

    # Return answer ...
    return midLon, midLat, maxDist
