#!/usr/bin/env python3

# Define function ...
def fillin_MultiPolygon(multipoly, fill, /, *, debug = False, eps = 1.0e-12, fillSpace = "EuclideanSpace", nIter = 100, prefix = ".", ramLimit = 1073741824, tol = 1.0e-10):
    """Fill in a MultiPolygon

    This function reads in a MultiPolygon, made up of Polygons (with an exterior
    and any number of interiors), that exists on the surface of the Earth and
    returns a MultiPolygon of the same MultiPolygon filled in by a constant
    distance: either in degrees in Euclidean space; or in metres in Geodesic
    space.

    Parameters
    ----------
    multipoly : shapely.geometry.multipolygon.MultiPolygon
        the MultiPolygon
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
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    fills : shapely.geometry.multipolygon.MultiPolygon
        the filled in MultiPolygon

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
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ..check import check
    from .fillin_Polygon import fillin_Polygon

    # Check argument ...
    if not isinstance(multipoly, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly\" is not a MultiPolygon") from None
    if debug:
        check(multipoly, prefix = prefix)

    # Initialize list ...
    polys = []

    # Loop over Polygons ...
    for poly in multipoly.geoms:
        # Append filled in Polygon to list ...
        polys.append(
            fillin_Polygon(
                poly,
                fill,
                    debug = debug,
                      eps = eps,
                fillSpace = fillSpace,
                    nIter = nIter,
                   prefix = prefix,
                 ramLimit = ramLimit,
                      tol = tol,
            )
        )

    # Convert list of Polygons to a (unified) MultiPolygon ...
    fills = shapely.ops.unary_union(polys)
    if debug:
        check(fills, prefix = prefix)

    # Return answer ...
    return fills
