#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs(
    lons,
    lats,
    /,
    *,
      conv = 1.0e3,
     debug = False,
       eps = 1.0e-12,
    method = "GeodesicBox",
     nIter = 10,
      nmax = 100,
       pad = 10.0e3,
):
    """Find the middle of some locations

    This function finds the middle of some locations such that the Geodesic
    distances to the West/East extremities are equalised and the Geodesic
    distances to the South/North extremities are equalised.

    Parameters
    ----------
    lons : numpy.ndarray
        the longitudes (in degrees)
    lats : numpy.ndarray
        the latitudes (in degrees)
    conv : float, optional
        the Geodesic distance that defines the middle as being converged (in
        metres)
    debug : bool, optional
        print debug messages
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    nIter : int, optional
        the maximum number of iterations
    nmax : int, optional
        the maximum number of the Vincenty formula iterations
    pad : float, optional
        the padding to add to the maximum Geodesic distance from the middle to
        the most extreme location (in metres)

    Returns
    -------
    midLon : float
        the middle longitude (in degrees)
    midLat : float
        the middle latitude (in degrees)
    maxDist : float
        the maximum Geodesic distance from the middle to the most extreme
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
    from .find_middle_of_locsSrc import find_middle_of_locs_euclideanBox
    from .find_middle_of_locsSrc import find_middle_of_locs_euclideanCircle
    from .find_middle_of_locsSrc import find_middle_of_locs_geodesicBox
    from .find_middle_of_locsSrc import find_middle_of_locs_geodesicCircle

    # **************************************************************************

    # Check arguments ...
    if not isinstance(lons, numpy.ndarray):
        raise TypeError("\"lons\" is not a NumPy array") from None
    if not isinstance(lats, numpy.ndarray):
        raise TypeError("\"lats\" is not a NumPy array") from None

    # **************************************************************************

    # Check what method the user wants ...
    match method:
        case "EuclideanBox":
            # Return answer ...
            return find_middle_of_locs_euclideanBox(
                lons,
                lats,
                debug = debug,
                  eps = eps,
                 nmax = nmax,
                  pad = pad,
            )
        case "EuclideanCircle":
            # Return answer ...
            return find_middle_of_locs_euclideanCircle(
                lons,
                lats,
                 conv = conv,
                debug = debug,
                  eps = eps,
                nIter = nIter,
                 nmax = nmax,
                  pad = pad,
            )
        case "GeodesicBox":
            # Return answer ...
            return find_middle_of_locs_geodesicBox(
                lons,
                lats,
                 conv = conv,
                debug = debug,
                  eps = eps,
                nIter = nIter,
                 nmax = nmax,
                  pad = pad,
            )
        case "GeodesicCircle":
            # Return answer ...
            return find_middle_of_locs_geodesicCircle(
                lons,
                lats,
                 conv = conv,
                debug = debug,
                  eps = eps,
                nIter = nIter,
                 nmax = nmax,
                  pad = pad,
            )
        case _:
            # Crash ...
            raise ValueError(f"\"method\" is an unexpected value ({repr(method)})") from None
