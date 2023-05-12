#!/usr/bin/env python3

# Define function ...
def en2ll_MultiPoint(multipoint1, /, *, debug = False, prefix = "."):
    """Transform a MultiPoint from Eastings/Northings to Longitudes/Latitudes

    This function reads in a MultiPoint whose coordinates are Eastings/Northings
    on the Ordnance Survey National Grid and returns a MultiPoint whose
    coordinates are Longitudes/Latitudes.

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
    from .en2ll_Point import en2ll_Point

    # Check argument ...
    if not isinstance(multipoint1, shapely.geometry.multipoint.MultiPoint):
        raise TypeError("\"multipoint1\" is not a MultiPoint") from None
    if debug:
        check(multipoint1, prefix = prefix)

    # Initialize list ...
    points = []

    # Loop over Points ...
    for point in multipoint1.geoms:
        # Append transformed Point to list ...
        points.append(
            en2ll_Point(
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
