def fillin_MultiLineString(multiline, fill, kwArgCheck = None, debug = False, fillSpace = "EuclideanSpace"):
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
            the Euclidean or Geodesic distance to fill in between each point within the shape by (in degrees or metres)
    debug : bool, optional
            print debug messages
    fillSpace : str, optional
            the geometric space to perform the filling in (either "EuclideanSpace" or "GeodesicSpace")

    Returns
    -------
    fills : shapely.geometry.multilinestring.MultiLineString
            the filled in MultiLineString
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .._debug import _debug
    from .fillin_LineString import fillin_LineString

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multiline, shapely.geometry.multilinestring.MultiLineString):
        raise TypeError("\"multiline\" is not a MultiLineString") from None
    if not multiline.is_valid:
        _debug(multiline)
        raise Exception(f"\"multiline\" is not a valid MultiLineString ({shapely.validation.explain_validity(multiline)})") from None
    if multiline.is_empty:
        raise Exception("\"multiline\" is an empty MultiLineString") from None

    # Initialize list ...
    lines = []

    # Loop over LineStrings ...
    for line in multiline.geoms:
        # Append filled in LineString to list ...
        lines.append(fillin_LineString(line, fill, debug = debug, fillSpace = fillSpace))

    # Convert list of LineStrings to a MultiLineString ...
    fills = shapely.geometry.multilinestring.MultiLineString(lines)

    # Clean up ...
    del lines

    # Check MultiLineString ...
    if not isinstance(fills, shapely.geometry.multilinestring.MultiLineString):
        raise TypeError("\"fills\" is not a MultiLineString") from None
    if not fills.is_valid:
        _debug(fills)
        raise Exception(f"\"fills\" is not a valid MultiLineString ({shapely.validation.explain_validity(fills)})") from None
    if fills.is_empty:
        raise Exception("\"fills\" is an empty MultiLineString") from None

    # Return answer ...
    return fills
