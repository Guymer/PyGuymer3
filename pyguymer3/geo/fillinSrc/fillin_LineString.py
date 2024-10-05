#!/usr/bin/env python3

# Define function ...
def fillin_LineString(
    line,
    fill,
    /,
    *,
        debug = __debug__,
          eps = 1.0e-12,
    fillSpace = "EuclideanSpace",
        nIter = 100,
       prefix = ".",
     ramLimit = 1073741824,
):
    """Fill in a LineString

    This function reads in a LineString that exists on the surface of the Earth
    and returns a LineString of the same LineString filled in by a constant
    distance: either in degrees in Euclidean space; or in metres in Geodesic
    space.

    Parameters
    ----------
    line : shapely.geometry.linestring.LineString
        the LineString
    fill : float
        the Euclidean or Geodesic distance to fill in between each point within
        the shape by (in degrees or metres)
    debug : bool, optional
        print debug messages
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)

    Returns
    -------
    fills : shapely.geometry.linestring.LineString
        the filled in LineString

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ..check import check
    from .fillin_CoordinateSequence import fillin_CoordinateSequence

    # **************************************************************************

    # Check argument ...
    assert isinstance(line, shapely.geometry.linestring.LineString), "\"line\" is not a LineString"
    if debug:
        check(line, prefix = prefix)

    # Return filled in LineString ...
    return fillin_CoordinateSequence(
        line.coords,
        fill,
            debug = debug,
              eps = eps,
        fillSpace = fillSpace,
            nIter = nIter,
           prefix = prefix,
         ramLimit = ramLimit,
    )
