#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs_geodesicBox(
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
    """Find the middle of some locations such that: a) the Geodesic distance to
    the most Northern point is the same as the Geodesic distance to the most
    Southern point; and b) the Geodesic distance to the most Eastern point is
    the same as the Geodesic distance to the most Western point.
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
        # Find the Geodesic distances to the western-most and eastern-most
        # locations ...
        maxWestDist = 0.0                                                       # [m]
        maxEastDist = 0.0                                                       # [m]
        for iLoc in range(lons.size):
            if lons[iLoc] < midLon:                                             # NOTE: Location is west of the middle.
                maxWestDist = max(
                    maxWestDist,
                    calc_dist_between_two_locs(
                        midLon,
                        midLat,
                        lons[iLoc],
                        midLat,
                         eps = eps,
                        nMax = nMax,
                    )[0]
                )                                                               # [m]
            elif lons[iLoc] > midLon:                                           # NOTE: Location is east of the middle.
                maxEastDist = max(
                    maxEastDist,
                    calc_dist_between_two_locs(
                        midLon,
                        midLat,
                        lons[iLoc],
                        midLat,
                         eps = eps,
                        nMax = nMax,
                    )[0]
                )                                                               # [m]
            else:
                pass

        # Find the Geodesic distances to the southern-most and northern-most
        # locations ...
        maxSouthDist = 0.0                                                      # [m]
        maxNorthDist = 0.0                                                      # [m]
        for iLoc in range(lons.size):
            if lats[iLoc] < midLat:                                             # NOTE: Location is south of the middle.
                maxSouthDist = max(
                    maxSouthDist,
                    calc_dist_between_two_locs(
                        midLon,
                        midLat,
                        midLon,
                        lats[iLoc],
                         eps = eps,
                        nMax = nMax,
                    )[0]
                )                                                               # [m]
            elif lats[iLoc] > midLat:                                           # NOTE: Location is north of the middle.
                maxNorthDist = max(
                    maxNorthDist,
                    calc_dist_between_two_locs(
                        midLon,
                        midLat,
                        midLon,
                        lats[iLoc],
                         eps = eps,
                        nMax = nMax,
                    )[0]
                )                                                               # [m]
            else:
                pass

        if debug:
            print(f"INFO: #{iIter + 1:,d}: {0.001 * maxWestDist:,.1f} km west ← middle → {0.001 * maxEastDist:,.1f} km east.")
            print(f"INFO: #{iIter + 1:,d}: {0.001 * maxSouthDist:,.1f} km south ↓ middle ↑ {0.001 * maxNorthDist:,.1f} km north.")

        # Initialize flag ...
        moved = False

        # Check if the middle needs moving west/east ...
        if 0.5 * (maxWestDist - maxEastDist) > conv:
            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * 0.5 * (maxWestDist - maxEastDist):,.1f} km towards the west ...")
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                270.0,
                0.5 * (maxWestDist - maxEastDist),
                 eps = eps,
                nMax = nMax,
            )                                                                   # [°], [°]
            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")
            moved = True
        elif 0.5 * (maxEastDist - maxWestDist) > conv:
            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * 0.5 * (maxEastDist - maxWestDist):,.1f} km towards the east ...")
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                90.0,
                0.5 * (maxEastDist - maxWestDist),
                 eps = eps,
                nMax = nMax,
            )                                                                   # [°], [°]
            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")
            moved = True
        else:
            pass

        # Check if the middle needs moving south/north ...
        if 0.5 * (maxSouthDist - maxNorthDist) > conv:
            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * 0.5 * (maxSouthDist - maxNorthDist):,.1f} km towards the south ...")
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                180.0,
                0.5 * (maxSouthDist - maxNorthDist),
                 eps = eps,
                nMax = nMax,
            )                                                                   # [°], [°]
            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")
            moved = True
        elif 0.5 * (maxNorthDist - maxSouthDist) > conv:
            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * 0.5 * (maxNorthDist - maxSouthDist):,.1f} km towards the north ...")
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                0.0,
                0.5 * (maxNorthDist - maxSouthDist),
                 eps = eps,
                nMax = nMax,
            )                                                                   # [°], [°]
            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")
            moved = True
        else:
            pass

        # Check if we can stop iterating ...
        if not moved:
            break

        # Stop if the end of the loop has been reached but the answer has not
        # converged ...
        if iIter == nIter - 1:
            raise Exception(f"failed to converge; the middle is currently ({midLon:.6f}°, {midLat:.6f}°); nIter = {nIter:d}") from None

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
