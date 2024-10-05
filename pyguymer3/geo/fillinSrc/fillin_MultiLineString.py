#!/usr/bin/env python3

# Define function ...
def fillin_MultiLineString(
    multiline,
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
    """Fill in a MultiLineString

    This function reads in a MultiLineString that exists on the surface of the
    Earth and returns a MultiLineString of the same MultiLineString filled in by
    a constant distance: either in degrees in Euclidean space; or in metres in
    Geodesic space.

    Parameters
    ----------
    line : shapely.geometry.multilinestring.MultiLineString
        the MultiLineString
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
    fills : shapely.geometry.multilinestring.MultiLineString
        the filled in MultiLineString

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
    from .fillin_LineString import fillin_LineString

    # **************************************************************************

    # Check argument ...
    assert isinstance(multiline, shapely.geometry.multilinestring.MultiLineString), "\"multiline\" is not a MultiLineString"
    if debug:
        check(multiline, prefix = prefix)

    # Initialize list ...
    lines = []

    # Loop over LineStrings ...
    for line in multiline.geoms:
        # Append filled in LineString to list ...
        lines.append(
            fillin_LineString(
                line,
                fill,
                    debug = debug,
                      eps = eps,
                fillSpace = fillSpace,
                    nIter = nIter,
                   prefix = prefix,
                 ramLimit = ramLimit,
            )
        )

    # Convert list of LineStrings to a MultiLineString ...
    fills = shapely.geometry.multilinestring.MultiLineString(lines)
    if debug:
        check(fills, prefix = prefix)

    # Return answer ...
    return fills
