#!/usr/bin/env python3

# Define function ...
def max_dist_geodesic(
    lons,
    lats,
    midLon,
    midLat,
    /,
    *,
      eps = 1.0e-12,
    nIter = 100,
):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from ..calc_dist_between_two_locs import calc_dist_between_two_locs

    # **************************************************************************

    # Check arguments ...
    assert isinstance(lons, numpy.ndarray), "\"lons\" is not a NumPy array"
    assert isinstance(lats, numpy.ndarray), "\"lats\" is not a NumPy array"

    # **************************************************************************

    # Find the maximum Geodesic distance from the middle to any location ...
    maxDist = 0.0                                                               # [m]
    for iLoc in range(lons.size):
        maxDist = max(
            maxDist,
            calc_dist_between_two_locs(
                midLon,
                midLat,
                lons[iLoc],
                lats[iLoc],
                  eps = eps,
                nIter = nIter,
            )[0],
        )                                                                       # [m]

    # Return answer ...
    return maxDist
