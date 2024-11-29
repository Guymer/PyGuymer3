#!/usr/bin/env python3

# Define function ...
def min_dist_euclidean(
    lons,
    lats,
    midLon,
    midLat,
    /,
):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # **************************************************************************

    # Check arguments ...
    assert isinstance(lons, numpy.ndarray), "\"lons\" is not a NumPy array"
    assert isinstance(lats, numpy.ndarray), "\"lats\" is not a NumPy array"

    # **************************************************************************

    # Find the minimum Euclidean distance from the middle to any location ...
    minDist = 1.0e9                                                             # [°]
    for iLoc in range(lons.size):
        minDist = min(
            minDist,
            numpy.hypot(
                lons[iLoc] - midLon,
                lats[iLoc] - midLat,
            ),
        )                                                                       # [°]

    # Return answer ...
    return minDist
