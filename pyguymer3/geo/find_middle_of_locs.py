#!/usr/bin/env python3

# Define function ...
def find_middle_of_locs(
    lons,
    lats,
    /,
    *,
           angConv = 0.1,
    attemptFortran = True,
              conv = 1.0e3,
             debug = __debug__,
               eps = 1.0e-12,
            method = "GeodesicBox",
            midLat = None,
            midLon = None,
              nAng = 9,
             nIter = 100,
           nRefine = 1,
               pad = 10.0e3,
          useSciPy = False,
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
        the angle change which classifies as converged (in degrees)
    attemptFortran : bool, optional
        attempt to use a f2py implementation
    conv : float, optional
        the distance that defines the middle as being converged (in degrees or
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
    nRefine : int, optional
        the number of refinements to make (each refinement halves the "conv"
        distance)
    pad : float, optional
        the padding to add to the maximum distance from the middle to the most
        extreme location (in degrees or metres)
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
    if attemptFortran:
        try:
            from ..f90 import funcs
            if debug:
                print("INFO: Will find the middle using FORTRAN.")
            fortran = True
        except ModuleNotFoundError:
            if debug:
                print("INFO: Will find the middle using Python (did not find FORTRAN module).")
            fortran = False
        except ImportError:
            if debug:
                print("INFO: Will find the middle using Python (error when attempting to import found FORTRAN).")
            fortran = False
    else:
        if debug:
            print("INFO: Will find the middle using Python (you told me not to attempt using FORTRAN).")
        fortran = False

    # **************************************************************************

    # Check arguments ...
    assert isinstance(lons, numpy.ndarray), "\"lons\" is not a NumPy array"
    assert isinstance(lats, numpy.ndarray), "\"lats\" is not a NumPy array"
    if lons.size == 1:
        if pad > 0.0:
            return lons[0], lats[0], pad
        return lons[0], lats[0], 0.0

    # **************************************************************************

    # Check what method the user wants ...
    match method:
        case "EuclideanBox":
            # Return answer ...
            # NOTE: The FORTRAN implementation does not support padding.
            if fortran:
                midLon, midLat, maxDist = funcs.find_middle_of_locs_euclideanbox(   # pylint: disable=E0606
                    lons,
                    lats,
                )
                if pad > 0.0:
                    maxDist += pad                                              # [°]
                return midLon, midLat, maxDist
            return find_middle_of_locs_euclideanBox(
                lons,
                lats,
                debug = debug,
                  pad = pad,
            )
        case "EuclideanCircle":
            # Return answer ...
            # NOTE: The FORTRAN implementation does not support padding.
            if fortran:
                midLon, midLat, maxDist = funcs.find_middle_of_locs_euclideancircle(    # pylint: disable=E0606
                    lons,
                    lats,
                    angConv,
                    debug,
                    conv,
                    nAng,
                    nIter,
                    nIter,
                    nRefine,
                )
                if pad > 0.0:
                    maxDist += pad                                              # [°]
                return midLon, midLat, maxDist
            return find_middle_of_locs_euclideanCircle(
                lons,
                lats,
                 angConv = angConv,
                    conv = conv,
                   debug = debug,
                 iRefine = 0,
                  midLat = midLat,
                  midLon = midLon,
                    nAng = nAng,
                   nIter = nIter,
                 nRefine = nRefine,
                     pad = pad,
                useSciPy = useSciPy,
            )
        case "GeodesicBox":
            # Return answer ...
            return find_middle_of_locs_geodesicBox(
                lons,
                lats,
                   conv = conv,
                  debug = debug,
                    eps = eps,
                iRefine = 0,
                 midLat = midLat,
                 midLon = midLon,
                  nIter = nIter,
                nRefine = nRefine,
                    pad = pad,
            )
        case "GeodesicCircle":
            # Return answer ...
            # NOTE: The FORTRAN implementation does not support padding.
            if fortran:
                midLon, midLat, maxDist = funcs.find_middle_of_locs_geodesiccircle( # pylint: disable=E0606
                    lons,
                    lats,
                    angConv,
                    debug,
                    conv,
                    eps,
                    nAng,
                    nIter,
                    nIter,
                    nIter,
                    nRefine,
                )
                if pad > 0.0:
                    maxDist += pad                                              # [m]
                return midLon, midLat, maxDist
            return find_middle_of_locs_geodesicCircle(
                lons,
                lats,
                 angConv = angConv,
                    conv = conv,
                   debug = debug,
                     eps = eps,
                 iRefine = 0,
                  midLat = midLat,
                  midLon = midLon,
                    nAng = nAng,
                   nIter = nIter,
                 nRefine = nRefine,
                     pad = pad,
                useSciPy = useSciPy,
            )
        case _:
            # Crash ...
            raise ValueError(f"\"method\" is an unexpected value ({repr(method)})") from None
