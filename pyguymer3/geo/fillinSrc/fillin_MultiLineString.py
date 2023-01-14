#!/usr/bin/env python3

# Define function ...
def fillin_MultiLineString(multiline, fill, kwArgCheck = None, debug = False, eps = 1.0e-12, fillSpace = "EuclideanSpace", nmax = 100, prefix = ".", ramLimit = 1073741824):
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

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multiline, shapely.geometry.multilinestring.MultiLineString):
        raise TypeError("\"multiline\" is not a MultiLineString") from None
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
                     nmax = nmax,
                   prefix = prefix,
                 ramLimit = ramLimit,
            )
        )

    # Convert list of LineStrings to a MultiLineString ...
    fills = shapely.geometry.multilinestring.MultiLineString(lines)
    if debug:
        check(fills, prefix = prefix)

    # Clean up ...
    del lines

    # Return answer ...
    return fills
