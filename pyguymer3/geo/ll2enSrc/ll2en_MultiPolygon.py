#!/usr/bin/env python3

# Define function ...
def ll2en_MultiPolygon(multipoly1, /, *, debug = False, prefix = ".", tol = 1.0e-10):
    """Transform a MultiPolygon from Longitudes/Latitudes to Eastings/Northings

    This function reads in a MultiPolygon whose coordinates are
    Longitudes/Latitudes and returns a MultiPolygon whose coordinates are
    Eastings/Northings on the Ordnance Survey National Grid.

    Parameters
    ----------
    multipoly1 : shapely.geometry.multipolygon.MultiPolygon
        the MultiPolygon
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    multipoly2 : shapely.geometry.multipolygon.MultiPolygon
        the transformed MultiPolygon

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
    from .ll2en_Polygon import ll2en_Polygon

    # Check argument ...
    if not isinstance(multipoly1, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly1\" is not a MultiPolygon") from None
    if debug:
        check(multipoly1, prefix = prefix)

    # Initialize list ...
    polys = []

    # Loop over Polygons ...
    for poly in multipoly1.geoms:
        # Append transformed Polygon to list ...
        polys.append(
            ll2en_Polygon(
                poly,
                 debug = debug,
                prefix = prefix,
                   tol = tol,
            )
        )

    # Convert list of Polygons to a (unified) MultiPolygon ...
    multipoly2 = shapely.ops.unary_union(polys)
    if debug:
        check(multipoly2, prefix = prefix)

    # Return answer ...
    return multipoly2
