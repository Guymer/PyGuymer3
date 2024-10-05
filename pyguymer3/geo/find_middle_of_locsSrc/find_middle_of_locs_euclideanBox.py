#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs_euclideanBox(
    lons,
    lats,
    /,
    *,
    debug = __debug__,
      eps = 1.0e-12,
    nIter = 100,
      pad = 0.1,
):
    """Find the middle of some locations such that: a) the Euclidean distance to
    the most Northern point is the same as the Euclidean distance to the most
    Southern point; and b) the Euclidean distance to the most Eastern point is
    the same as the Euclidean distance to the most Western point.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from ..max_dist import max_dist

    # **************************************************************************

    # Check arguments ...
    assert isinstance(lons, numpy.ndarray), "\"lons\" is not a NumPy array"
    assert isinstance(lats, numpy.ndarray), "\"lats\" is not a NumPy array"

    # **************************************************************************

    # Calculate the middle of the Euclidean bounding box ...
    midLon = 0.5 * (lons.min() + lons.max())                                    # [°]
    midLat = 0.5 * (lats.min() + lats.max())                                    # [°]

    if debug:
        print(f"INFO: The middle is ({midLon:.6f}°, {midLat:.6f}°).")

    # Find the maximum Euclidean distance from the middle to any location ...
    maxDist = max_dist(
        lons,
        lats,
        midLon,
        midLat,
          eps = eps,
        nIter = nIter,
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
