#!/usr/bin/env python3

# Define function ...
def fillin_LinearRing(ring, fill, /, *, debug = False, eps = 1.0e-12, fillSpace = "EuclideanSpace", nmax = 100, prefix = ".", ramLimit = 1073741824):
    """Fill in a LinearRing

    This function reads in a LinearRing that exists on the surface of the Earth
    and returns a LineString of the same LinearRing filled in by a constant
    distance: either in degrees in Euclidean space; or in metres in Geodesic
    space.

    Parameters
    ----------
    ring : shapely.geometry.polygon.LinearRing
        the LinearRing
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
    nmax : int, optional
        the maximum number of the Vincenty formula iterations
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)

    Returns
    -------
    fills : shapely.geometry.linestring.LineString
        the filled in LinearRing

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
    if not isinstance(ring, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"ring\" is not a LinearRing") from None
    if debug:
        check(ring, prefix = prefix)

    # Return filled in LinearRing ...
    return fillin_CoordinateSequence(
        ring.coords,
        fill,
            debug = debug,
              eps = eps,
        fillSpace = fillSpace,
             nmax = nmax,
           prefix = prefix,
         ramLimit = ramLimit,
    )
