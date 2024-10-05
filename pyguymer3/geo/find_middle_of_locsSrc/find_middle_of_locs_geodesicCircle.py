#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs_geodesicCircle(
    lons,
    lats,
    /,
    *,
     angConv = 0.1,
        conv = 1.0e3,
       debug = __debug__,
         eps = 1.0e-12,
     iRefine = 0,
      midLat = None,
      midLon = None,
        nAng = 9,
       nIter = 100,
     nRefine = 1,
         pad = 10.0e3,
    useSciPy = False,
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
    from ..calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
    from ..find_min_max_dist_bearing import find_min_max_dist_bearing
    from ..max_dist import max_dist
    from ...consts import RESOLUTION_OF_EARTH

    # **************************************************************************

    # Check arguments ...
    assert isinstance(lons, numpy.ndarray), "\"lons\" is not a NumPy array"
    assert isinstance(lats, numpy.ndarray), "\"lats\" is not a NumPy array"

    # **************************************************************************

    # Calculate the middle of the Euclidean bounding box to use as a decent
    # starting guess (if the user has not provided one) ...
    if midLon is None or midLat is None:
        midLon, midLat, _ = find_middle_of_locs_euclideanBox(
            lons,
            lats,
            debug = debug,
              eps = eps,
            nIter = nIter,
              pad = -1.0,
        )                                                                       # [°], [°]

    # Find the maximum Geodesic distance from the middle to any location ...
    maxDist = max_dist(
        lons,
        lats,
        midLon,
        midLat,
          eps = eps,
        nIter = nIter,
        space = "GeodesicSpace",
    )                                                                           # [m]

    if debug:
        print(f"INFO: The initial middle is ({midLon:.6f}°, {midLat:.6f}°) and the initial maximum Geodesic distance is {0.001 * maxDist:,.1f} km.")

    # **************************************************************************

    # Check if the input is already converged or if the user wants to use SciPy ...
    if maxDist < conv:
        pass
    elif useSciPy:
        # Use SciPy to find the minimum maximum Geodesic distance ...
        ans = scipy.optimize.minimize(
            lambda x: max_dist(
                lons,
                lats,
                x[0],
                x[1],
                  eps = eps,
                nIter = nIter,
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
            nIter = nIter,
            space = "GeodesicSpace",
        )                                                                       # [m]

        if debug:
            print(f"INFO: The middle is finally ({midLon:.6f}°, {midLat:.6f}°) and the maximum Geodesic distance is finally {0.001 * maxDist:,.1f} km.")
    else:
        # Loop over iterations ...
        for iIter in range(nIter):
            # Find the angle towards the minimum maximum Geodesic distance ...
            minAng = find_min_max_dist_bearing(
                midLon,
                midLat,
                lons,
                lats,
                     angConv = angConv,
                angHalfRange = 180.0,
                       debug = debug,
                        dist = conv,
                         eps = eps,
                       first = True,
                       iIter = 0,
                        nAng = nAng,
                       nIter = nIter,
                       space = "GeodesicSpace",
                    startAng = 180.0,
            )                                                                   # [°]

            if debug:
                print(f"INFO: #{iIter + 1:,d}/{nIter:,d}: Moving middle {0.001 * conv:,.1f} km towards {minAng:.1f}° ...")

            # Find the new location ...
            newMidLon, newMidLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                minAng,
                conv,
                  eps = eps,
                nIter = nIter,
            )                                                                   # [°], [°]

            # Find the maximum Geodesic distance from the middle to any location ...
            newMaxDist = max_dist(
                lons,
                lats,
                newMidLon,
                newMidLat,
                  eps = eps,
                nIter = nIter,
                space = "GeodesicSpace",
            )                                                                   # [m]

            # Stop iterating if the answer isn't getting any better ...
            if newMaxDist > maxDist:
                if debug:
                    print(f"INFO: #{iIter + 1:,d}/{nIter:,d}: The middle is finally ({midLon:.6f}°, {midLat:.6f}°) and the maximum Geodesic distance is finally {0.001 * maxDist:,.1f} km.")
                break

            # Update values ...
            maxDist = newMaxDist                                                # [m]
            midLon = newMidLon                                                  # [°]
            midLat = newMidLat                                                  # [°]

            # Stop if the end of the loop has been reached but the answer has
            # not converged ...
            if iIter == nIter - 1:
                raise Exception(f"failed to converge; the middle is currently ({midLon:.6f}°, {midLat:.6f}°); nIter = {nIter:,d}") from None

            if debug:
                print(f"INFO: #{iIter + 1:,d}/{nIter:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°) and the maximum Geodesic distance is now {0.001 * maxDist:,.1f} km.")

    # **************************************************************************

    # Check if a padding needs to be added ...
    if pad > 0.0:
        # Add padding ...
        maxDist += pad                                                          # [m]

        if debug:
            print(f"INFO: Maximum (padded) Geodesic distance is finally {0.001 * maxDist:,.1f} km.")

    # Return answer ...
    if iRefine == nRefine - 1:
        return midLon, midLat, maxDist
    return find_middle_of_locs_geodesicCircle(
        lons,
        lats,
         angConv = angConv,
            conv = 0.5 * conv,
           debug = debug,
             eps = eps,
         iRefine = iRefine + 1,
          midLat = midLat,
          midLon = midLon,
            nAng = nAng,
           nIter = nIter,
         nRefine = nRefine,
             pad = pad,
        useSciPy = useSciPy,
    )
