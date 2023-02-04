#!/usr/bin/env python3

# Define function ...
def fillin_LineString(line, fill, /, *, debug = False, eps = 1.0e-12, fillSpace = "EuclideanSpace", nmax = 100, prefix = ".", ramLimit = 1073741824):
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
        the Euclidean or Geodetic distance to fill in between each point within
        the shape by (in degrees or metres)
    debug : bool, optional
        print debug messages
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    ramLimit : int, optional
        the maximum RAM usage of each "large" array, in bytes

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

    # Check argument ...
    if not isinstance(line, shapely.geometry.linestring.LineString):
        raise TypeError("\"line\" is not a LineString") from None
    if debug:
        check(line, prefix = prefix)

    # Return filled in LineString ...
    return fillin_CoordinateSequence(
        line.coords,
        fill,
            debug = debug,
              eps = eps,
        fillSpace = fillSpace,
             nmax = nmax,
           prefix = prefix,
         ramLimit = ramLimit,
    )
