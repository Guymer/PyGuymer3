#!/usr/bin/env python3

# Define function ...
def ll2en_MultiPoint(
    multipoint1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
):
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
    prefix : str, optional
        change the name of the output debugging CSVs

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
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ..check import check
    from .ll2en_Point import ll2en_Point

    # **************************************************************************

    # Check argument ...
    assert isinstance(multipoint1, shapely.geometry.multipoint.MultiPoint), "\"multipoint1\" is not a MultiPoint"
    if debug:
        check(multipoint1, prefix = prefix)

    # Initialize list ...
    points = []

    # Loop over Points ...
    for point in multipoint1.geoms:
        # Append transformed Point to list ...
        points.append(
            ll2en_Point(
                point,
                 debug = debug,
                prefix = prefix,
            )
        )

    # Convert list of Points to a (unified) MultiPoint ...
    multipoint2 = shapely.ops.unary_union(points)
    if debug:
        check(multipoint2, prefix = prefix)

    # Return answer ...
    return multipoint2
