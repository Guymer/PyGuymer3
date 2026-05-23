#!/usr/bin/env python3

# Define function ...
def max_dist(
    lons,
    lats,
    midLon,
    midLat,
    /,
    *,
    attemptFortran = True,
             debug = __debug__,
               eps = 1.0e-12,
             nIter = 100,
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
    if attemptFortran:
        try:
            from ..f90 import funcs
            if debug:
                print("INFO: Will find the maximum using FORTRAN.")
            fortran = True
        except ModuleNotFoundError:
            if debug:
                print("INFO: Will find the maximum using Python (did not find FORTRAN module).")
            fortran = False
        except ImportError:
            if debug:
                print("INFO: Will find the maximum using Python (error when attempting to import found FORTRAN).")
            fortran = False
    else:
        if debug:
            print("INFO: Will find the maximum using Python (you told me not to attempt using FORTRAN).")
        fortran = False

    # **************************************************************************

    # Check arguments ...
    assert isinstance(lons, numpy.ndarray), "\"lons\" is not a NumPy array"
    assert isinstance(lats, numpy.ndarray), "\"lats\" is not a NumPy array"

    # **************************************************************************

    # Check what space the user wants to measure in ...
    match space:
        case "EuclideanSpace":
            # Check arguments ...
            assert eps is None, "\"eps\" is not None but \"space\" is \"EuclideanSpace\""
            assert nIter is None, "\"nIter\" is not None but \"space\" is \"EuclideanSpace\""

            # Return answer ...
            # NOTE: The FORTRAN implementation does not support padding.
            if fortran:
                return funcs.max_dist_euclideanspace(                           # pylint: disable=E0606
                    midlon = midLon,
                    midlat = midLat,
                      lons = lons,
                      lats = lats,
                )
            return max_dist_euclidean(
                lons,
                lats,
                midLon,
                midLat,
            )
        case "GeodesicSpace":
            # Return answer ...
            # NOTE: The FORTRAN implementation does not support padding.
            if fortran:
                return funcs.max_dist_geodesicspace(                            # pylint: disable=E0606
                    midlon = midLon,
                    midlat = midLat,
                      lons = lons,
                      lats = lats,
                       eps = eps,
                      nmax = nIter,
                )
            return max_dist_geodesic(
                lons,
                lats,
                midLon,
                midLat,
                  eps = eps,
                nIter = nIter,
            )
        case _:
            # Crash ...
            raise ValueError(f"\"space\" is an unexpected value ({repr(space)})") from None
