#!/usr/bin/env python3

# Define function ...
def max_dist(
    lons,
    lats,
    midLon,
    midLat,
    /,
    *,
      eps = 1.0e-12,
     nMax = 100,
    space = "EuclideanSpace",
):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .max_distSrc import max_dist_euclidean
    from .max_distSrc import max_dist_geodesic

    # **************************************************************************

    # Check arguments ...
    if not isinstance(lons, numpy.ndarray):
        raise TypeError("\"lons\" is not a NumPy array") from None
    if not isinstance(lats, numpy.ndarray):
        raise TypeError("\"lats\" is not a NumPy array") from None

    # **************************************************************************

    # Check what space the user wants to measure in ...
    match space:
        case "EuclideanSpace":
            # Return answer ...
            return max_dist_euclidean(
                lons,
                lats,
                midLon,
                midLat,
            )
        case "GeodesicSpace":
            # Return answer ...
            return max_dist_geodesic(
                lons,
                lats,
                midLon,
                midLat,
                 eps = eps,
                nMax = nMax,
            )
        case _:
            # Crash ...
            raise ValueError(f"\"space\" is an unexpected value ({repr(space)})") from None
