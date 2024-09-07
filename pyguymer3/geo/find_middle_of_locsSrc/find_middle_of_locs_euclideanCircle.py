#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs_euclideanCircle(
    lons,
    lats,
    /,
    *,
     conv = 0.01,
    debug = False,
      eps = 1.0e-12,
    nIter = 10,
     nMax = 100,
      pad = 0.1,
):
    """Find the middle of some locations such that they are encompassed by the
    smallest Euclidean circle possible.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .find_middle_of_locs_euclideanBox import find_middle_of_locs_euclideanBox
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
        pntN = (pntC[0], pntC[1] + conv)                                        # [°], [°]
        pntNN = (pntC[0], pntC[1] + 2.0 * conv)                                 # [°], [°]
        pntE = (pntC[0] + conv, pntC[1])                                        # [°], [°]
        pntEE = (pntC[0] + 2.0 * conv, pntC[1])                                 # [°], [°]
        pntS = (pntC[0], pntC[1] - conv)                                        # [°], [°]
        pntSS = (pntC[0], pntC[1] - 2.0 * conv)                                 # [°], [°]
        pntW = (pntC[0] - conv, pntC[1])                                        # [°], [°]
        pntWW = (pntC[0] - 2.0 * conv, pntC[1])                                 # [°], [°]

        # Find the maximum Euclidean distance from the points to any location ...
        distC = max_dist(
            lons,
            lats,
            pntC[0],
            pntC[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]
        distN = max_dist(
            lons,
            lats,
            pntN[0],
            pntN[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]
        distNN = max_dist(
            lons,
            lats,
            pntNN[0],
            pntNN[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]
        distE = max_dist(
            lons,
            lats,
            pntE[0],
            pntE[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]
        distEE = max_dist(
            lons,
            lats,
            pntEE[0],
            pntEE[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]
        distS = max_dist(
            lons,
            lats,
            pntS[0],
            pntS[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]
        distSS = max_dist(
            lons,
            lats,
            pntSS[0],
            pntSS[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]
        distW = max_dist(
            lons,
            lats,
            pntW[0],
            pntW[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]
        distWW = max_dist(
            lons,
            lats,
            pntWW[0],
            pntWW[1],
              eps = eps,
             nMax = nMax,
            space = "EuclideanSpace",
        )                                                                       # [°]

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

        # Calculate the Euclidean distance and Euclidean bearing to the guess of
        # the local minimum ...
        dist = numpy.hypot(
            pntGuess[1] - pntC[1],
            pntGuess[0] - pntC[0],
        )                                                                       # [°]
        bear = (
            360.0 - numpy.degrees(
                numpy.arctan2(
                    pntGuess[1] - pntC[1],
                    pntGuess[0] - pntC[0],
                ) % (2.0 * numpy.pi)
            ) + 90.0
        ) % 360.0                                                               # [°]

        # Check if we can stop iterating ...
        if dist < conv:
            break

        # Stop if the end of the loop has been reached but the answer has not
        # converged ...
        if iIter == nIter - 1:
            raise Exception(f"failed to converge; the middle is currently ({midLon:.6f}°, {midLat:.6f}°); nIter = {nIter:,d}") from None

        if debug:
            print(f"INFO: #{iIter + 1:,d}/{nIter:,d}: Moving middle {conv:.6f}° towards {bear:.1f}° ...")

        # Update the middle location ...
        midLon += conv * numpy.sin(numpy.radians(bear))                         # [°]
        midLat += conv * numpy.cos(numpy.radians(bear))                         # [°]

        if debug:
            print(f"INFO: #{iIter + 1:,d}/{nIter:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")

    # Find the maximum Euclidean distance from the middle to any location ...
    maxDist = max_dist(
        lons,
        lats,
        midLon,
        midLat,
          eps = eps,
         nMax = nMax,
        space = "EuclideanSpace",
    )                                                                           # [°]

    if debug:
        print(f"INFO: Maximum Euclidean distance is {maxDist:.6f}°.")

    # **************************************************************************

    # Check if a padding needs to be added ...
    if pad > 0.0:
        # Add padding ...
        maxDist += pad                                                          # [°]

        if debug:
            print(f"INFO: Maximum (padded) Euclidean distance is {maxDist:.6f}°.")

    # Return answer ...
    return midLon, midLat, maxDist
