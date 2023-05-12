#!/usr/bin/env python3

# Define function ...
def ll2en_MultiLineString(multiline1, /, *, debug = False, prefix = "."):
    """Transform a MultiLineString from Longitudes/Latitudes to
    Eastings/Northings

    This function reads in a MultiLineString whose coordinates are
    Longitudes/Latitudes and returns a MultiLineString whose coordinates are
    Eastings/Northings on the Ordnance Survey National Grid.

    Parameters
    ----------
    multiline1 : shapely.geometry.multilinestring.MultiLineString
        the MultiLineString
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs

    Returns
    -------
    multiline2 : shapely.geometry.multilinestring.MultiLineString
        the transformed MultiLineString

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
    from .ll2en_LineString import ll2en_LineString

    # Check argument ...
    if not isinstance(multiline1, shapely.geometry.multilinestring.MultiLineString):
        raise TypeError("\"multiline1\" is not a MultiLineString") from None
    if debug:
        check(multiline1, prefix = prefix)

    # Initialize list ...
    lines = []

    # Loop over LineStrings ...
    for line in multiline1.geoms:
        # Append transformed LineString to list ...
        lines.append(
            ll2en_LineString(
                line,
                debug = debug,
            )
        )

    # Convert list of LineStrings to a (unified) MultiLineString ...
    multiline2 = shapely.ops.unary_union(lines)
    if debug:
        check(multiline2, prefix = prefix)

    # Return answer ...
    return multiline2
