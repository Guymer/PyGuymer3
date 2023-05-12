#!/usr/bin/env python3

# Define function ...
def ll2en_MultiPoint(multipoint1, /, *, debug = False):
    """Transform a MultiPoint from Longitudes/Latitudes to Eastings/Northings

    This function reads in a MultiPoint whose coordinates are
    Longitudes/Latitudes and returns a MultiPoint whose coordinates are
    Eastings/Northings on the Ordnance Survey National Grid.

    Parameters
    ----------
    multipoint1 : shapely.geometry.multipoint.MultiPoint
        the MultiPoint
    debug : bool, optional
        print debug messages

    Returns
    -------
    multipoint2 : shapely.geometry.multipoint.MultiPoint
        the transformed MultiPoint

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
    from .ll2en_Point import ll2en_Point

    # Check argument ...
    if not isinstance(multipoint1, shapely.geometry.multipoint.MultiPoint):
        raise TypeError("\"multipoint1\" is not a MultiPoint") from None
    if debug:
        check(multipoint1)

    # Initialize list ...
    points = []

    # Loop over Points ...
    for point in multipoint1.geoms:
        # Append transformed Point to list ...
        points.append(
            ll2en_Point(
                point,
                debug = debug,
            )
        )

    # Convert list of Points to a (unified) MultiPoint ...
    multipoint2 = shapely.ops.unary_union(points)
    if debug:
        check(multipoint2)

    # Return answer ...
    return multipoint2
