#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs(lons, lats, /, *, debug = False, eps = 1.0e-12, nIter = 10, nmax = 100, pad = 10.0e3, tol = 1.0e3):
    """Find the middle of some locations

    This function finds the middle of some locations such that the Geodetic
    distances to the West/East extremities are equalised and the Geodetic
    distances to the South/North extremities are equalised.

    Parameters
    ----------
    lons : numpy.ndarray
        the longitudes (in degrees)
    lats : numpy.ndarray
        the latitudes (in degrees)
    debug : bool, optional
        print debug messages
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    nIter : int, optional
        the maximum number of iterations
    nmax : int, optional
        the maximum number of the Vincenty formula iterations
    pad : float, optional
        the padding to add to the maximum Geodetic distance from the middle to
        the most extreme location (in metres)
    tol : float, optional
        the Geodetic distance that defines the middle as being converged (in
        metres)

    Returns
    -------
    midLon : float
        the middle longitude (in degrees)
    midLat : float
        the middle latitude (in degrees)
    maxDist : float
        the maximum Geodetic distance from the middle to the most extreme
        location (in metres)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .calc_dist_between_two_locs import calc_dist_between_two_locs
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist

    # Check arguments ...
    if not isinstance(lons, numpy.ndarray):
        raise TypeError("\"lons\" is not a NumPy array") from None
    if not isinstance(lats, numpy.ndarray):
        raise TypeError("\"lats\" is not a NumPy array") from None

    # Guess the initial middle ...
    midLon = 0.5 * (lons.min() + lons.max())                                    # [°]
    midLat = 0.5 * (lats.min() + lats.max())                                    # [°]

    if debug:
        print(f"INFO: The initial middle is ({midLon:.6f}°, {midLat:.6f}°).")

    # Loop over iterations ...
    for iIter in range(nIter):
        # Find the distances to the western-most and eastern-most locations ...
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
                        nmax = nmax,
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
                        nmax = nmax,
                    )[0]
                )                                                               # [m]
            else:
                pass

        # Find the distances to the southern-most and northern-most locations ...
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
                        nmax = nmax,
                    )[0]
                )                                                               # [m]
            elif lats[iLoc] > midLon:                                           # NOTE: Location is north of the middle.
                maxNorthDist = max(
                    maxNorthDist,
                    calc_dist_between_two_locs(
                        midLon,
                        midLat,
                        midLon,
                        lats[iLoc],
                         eps = eps,
                        nmax = nmax,
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
        if 0.5 * (maxWestDist - maxEastDist) > tol:
            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * 0.5 * (maxWestDist - maxEastDist):,.1f} km west ...")
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                270.0,
                0.5 * (maxWestDist - maxEastDist),
                 eps = eps,
                nmax = nmax,
            )                                                                   # [°], [°]
            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")
            moved = True
        elif 0.5 * (maxEastDist - maxWestDist) > tol:
            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * 0.5 * (maxEastDist - maxWestDist):,.1f} km east ...")
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                90.0,
                0.5 * (maxEastDist - maxWestDist),
                 eps = eps,
                nmax = nmax,
            )                                                                   # [°], [°]
            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")
            moved = True
        else:
            pass

        # Check if the middle needs moving south/north ...
        if 0.5 * (maxSouthDist - maxNorthDist) > tol:
            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * 0.5 * (maxSouthDist - maxNorthDist):,.1f} km south ...")
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                180.0,
                0.5 * (maxSouthDist - maxNorthDist),
                 eps = eps,
                nmax = nmax,
            )                                                                   # [°], [°]
            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")
            moved = True
        elif 0.5 * (maxNorthDist - maxSouthDist) > tol:
            if debug:
                print(f"INFO: #{iIter + 1:,d}: Moving middle {0.001 * 0.5 * (maxNorthDist - maxSouthDist):,.1f} km north ...")
            midLon, midLat, _ = calc_loc_from_loc_and_bearing_and_dist(
                midLon,
                midLat,
                0.0,
                0.5 * (maxNorthDist - maxSouthDist),
                 eps = eps,
                nmax = nmax,
            )                                                                   # [°], [°]
            if debug:
                print(f"INFO: #{iIter + 1:,d}: The middle is now ({midLon:.6f}°, {midLat:.6f}°).")
            moved = True
        else:
            pass

        # Check if we can stop iterating ...
        if not moved:
            break

    # Find the maximum distance from the middle to any location ...
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
                nmax = nmax,
            )[0],
        )                                                                       # [m]

    if debug:
        print(f"INFO: Maximum distance is {0.001 * maxDist:,.1f} km.")

    # Check if a padding needs to be added ...
    if pad > 0.0:
        # Add padding ...
        maxDist += pad                                                          # [m]

        if debug:
            print(f"INFO: Maximum (padded) distance is {0.001 * maxDist:,.1f} km.")

    # Return answer ...
    return midLon, midLat, maxDist
