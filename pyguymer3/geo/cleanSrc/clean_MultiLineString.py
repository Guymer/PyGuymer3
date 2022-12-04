def clean_MultiLineString(multiline, kwArgCheck = None, debug = False, prefix = ".", tol = 1.0e-10):
    """Clean a MultiLineString

    This function cleans a MultiLineString by removing bad points.

    Parameters
    ----------
    line : shapely.geometry.multilinestring.MultiLineString
        the MultiLineString
    debug : bool, optional
        print debug messages
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    cleans : shapely.geometry.multilinestring.MultiLineString
        the cleaned MultiLineString

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
    from .clean_LineString import clean_LineString

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multiline, shapely.geometry.multilinestring.MultiLineString):
        raise TypeError("\"multiline\" is not a MultiLineString") from None

    # Initialize list ...
    lines = []

    # Loop over LineStrings ...
    for line in multiline.geoms:
        # Append cleaned LineString to list ...
        lines.append(
            clean_LineString(
                line,
                 debug = debug,
                prefix = prefix,
                   tol = tol,
            )
        )

    # Convert list of LineStrings to a MultiLineString ...
    cleans = shapely.geometry.multilinestring.MultiLineString(lines)
    if debug:
        check(cleans, prefix = prefix)

    # Clean up ...
    del lines

    # Return answer ...
    return cleans
