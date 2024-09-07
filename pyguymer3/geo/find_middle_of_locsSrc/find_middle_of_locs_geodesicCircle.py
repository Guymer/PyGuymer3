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
    useSciPy = True,
):
    """Find the middle of some locations such that they are encompassed by the
    smallest Geodesic circle possible.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import scipy
    except:
        raise Exception("\"scipy\" is not installed; run \"pip install --user scipy\"") from None

    # Import sub-functions ...
    from .find_middle_of_locs_euclideanBox import find_middle_of_locs_euclideanBox
    from ..calc_dist_between_two_locs import calc_dist_between_two_locs
    from ..calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
    from ..max_dist import max_dist
    from ...consts import RESOLUTION_OF_EARTH

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
        print(f"INFO: The initial middle is ({midLon:.6f}°, {midLat:.6f}°) and the initial maximum Geodesic distance is {0.001 * maxDist:,.1f} km.")

    # **************************************************************************

    # Check if the input is already converged or if the user wants to use SciPy ...
    if maxDist < conv:
        pass
    elif useSciPy:
        # Use SciPy to minimum the maximum Geodesic distance ...
        ans = scipy.optimize.minimize(
            lambda x: max_dist(
                lons,
                lats,
                x[0],
                x[1],
                  eps = eps,
                 nMax = nMax,
                space = "GeodesicSpace",
            ),
            [midLon, midLat],
            bounds = [
                (-180.0, +180.0),
                ( -90.0,  +90.0),
            ],
             method = "L-BFGS-B",
            options = {
                   "disp" : debug,
                "maxiter" : nIter,
            },
                tol = conv / RESOLUTION_OF_EARTH,
        )
        if not ans.success:
            print(lons)
            print(lats)
            print(ans)
            raise Exception("failed to converge") from None

        # Update the middle location ...
        midLon = ans.x[0]                                                       # [°]
        midLat = ans.x[1]                                                       # [°]

        # Find the maximum Geodesic distance from the middle to any location ...
        maxDist = max_dist(
            lons,
            lats,
            midLon,
            midLat,
              eps = eps,
             nMax = nMax,
            space = "GeodesicSpace",
        )                                                                       # [m]

        if debug:
            print(f"INFO: The middle is now ({midLon:.6f}°, {midLat:.6f}°) and the maximum Geodesic distance is now {0.001 * maxDist:,.1f} km.")
    else:
        # Loop over iterations ...
        for iIter in range(nIter):
            # Create points South/North of the central point, find the maximum
            # Geodesic distance from each point to any location and fit a
            # polynomial (degree 2) to the data ...
            pnts = []                                                           # [°], [°]
            for iPoint in range(-12, 13):
                pnts.append(
                    calc_loc_from_loc_and_bearing_and_dist(
                        midLon,
                        midLat,
                        0.0,
                        0.25 * float(iPoint) * conv,
                         eps = eps,
                        nMax = nMax,
                    )[:2]
                )                                                               # [°], [°]
            dists = []                                                          # [m]
            for pnt in pnts:
                dists.append(
                    max_dist(
                        lons,
                        lats,
                        pnt[0],
                        pnt[1],
                          eps = eps,
                         nMax = nMax,
                        space = "GeodesicSpace",
                    )
                )                                                               # [m]
            eqnSN = numpy.polynomial.Polynomial.fit(
                [pnt[1] for pnt in pnts],
                dists,
                   deg = 2,
                domain = [-90.0, +90.0],
                symbol = "y",
            )

            # Create points West/East of the central point, find the maximum
            # Geodesic distance from each point to any location and fit a
            # polynomial (degree 2) to the data ...
            pnts = []                                                           # [°], [°]
            for iPoint in range(-12, 13):
                pnts.append(
                    calc_loc_from_loc_and_bearing_and_dist(
                        midLon,
                        midLat,
                        90.0,
                        0.25 * float(iPoint) * conv,
                         eps = eps,
                        nMax = nMax,
                    )[:2]
                )                                                               # [°], [°]
            dists = []                                                          # [m]
            for pnt in pnts:
                dists.append(
                    max_dist(
                        lons,
                        lats,
                        pnt[0],
                        pnt[1],
                          eps = eps,
                         nMax = nMax,
                        space = "GeodesicSpace",
                    )
                )                                                               # [m]
            eqnWE = numpy.polynomial.Polynomial.fit(
                [pnt[0] for pnt in pnts],
                dists,
                   deg = 2,
                domain = [-180.0, +180.0],
                symbol = "x",
            )

            # Find the roots of the two polynomials and use them to guess where
            # the local minimum is (note that the roots are not clipped to be in
            # the domain, so I must do that myself) ...
            pntGuess = (
                max(-180.0, min(180.0, eqnWE.deriv().roots()[0])),
                max( -90.0, min( 90.0, eqnSN.deriv().roots()[0])),
            )                                                                   # [°], [°]

            # Calculate the Geodesic distance and Geodesic bearing to the guess
            # of the local minimum ...
            dist, bear, _ = calc_dist_between_two_locs(
                midLon,
                midLat,
                pntGuess[0],
                pntGuess[1],
                 eps = eps,
                nMax = nMax,
            )                                                                   # [m], [°]

            # Check if we can stop iterating ...
            if dist < conv:
                break

            # Stop if the end of the loop has been reached but the answer has
            # not converged ...
            if iIter == nIter - 1:
                raise Exception(f"failed to converge; the middle is currently ({midLon:.6f}°, {midLat:.6f}°); nIter = {nIter:d}") from None

            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * conv:,.1f} km towards {bear:.1f}° ...")

            # Update the middle location ...
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                bear,
                conv,
                 eps = eps,
                nMax = nMax,
            )                                                                   # [°], [°]

            # Find the maximum Geodesic distance from the middle to any location ...
            maxDist = max_dist(
                lons,
                lats,
                midLon,
                midLat,
                  eps = eps,
                 nMax = nMax,
                space = "GeodesicSpace",
            )                                                                   # [m]

            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°), the maximum Geodesic distance is now {0.001 * maxDist:,.1f} km and the guess of the final location is now ({pntGuess[0]:.6f}°, {pntGuess[1]:.6f}°).")

    # **************************************************************************

    # Check if a padding needs to be added ...
    if pad > 0.0:
        # Add padding ...
        maxDist += pad                                                          # [m]

        if debug:
            print(f"INFO: Maximum (padded) Geodesic distance is {0.001 * maxDist:,.1f} km.")

    # Return answer ...
    return midLon, midLat, maxDist
