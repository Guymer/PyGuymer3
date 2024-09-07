#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs(
    lons,
    lats,
    /,
    *,
     angConv = 0.1,
        conv = 1.0e3,
       debug = False,
         eps = 1.0e-12,
      method = "GeodesicBox",
        nAng = 9,
       nIter = 100,
         pad = 10.0e3,
    useSciPy = True,
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
    angConv : float, optional
        the angle change which classifies as converged
    conv : float, optional
        the Geodesic distance that defines the middle as being converged (in
        metres)
    debug : bool, optional
        print debug messages
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    method : str, optional
        the method for finding the middle of the locations
    nAng : int, optional
        the number of angles around the middle location to search over
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    pad : float, optional
        the padding to add to the maximum Geodesic distance from the middle to
        the most extreme location (in metres)
    useSciPy : bool, optional
        use "scipy.optimize.minimize" or my own minimizer

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
    if lons.size == 1:
        if pad > 0.0:
            return lons[0], lats[0], pad
        return lons[0], lats[0], 0.0

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
                nIter = nIter,
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
                  pad = pad,
            )
        case "GeodesicCircle":
            # Return answer ...
            return find_middle_of_locs_geodesicCircle(
                lons,
                lats,
                 angConv = angConv,
                    conv = conv,
                   debug = debug,
                     eps = eps,
                    nAng = nAng,
                   nIter = nIter,
                     pad = pad,
                useSciPy = useSciPy,
            )
        case _:
            # Crash ...
            raise ValueError(f"\"method\" is an unexpected value ({repr(method)})") from None
