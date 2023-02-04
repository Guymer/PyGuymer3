#!/usr/bin/env python3

# Define function ...
def clean_MultiPolygon(multipoly, /, *, debug = False, prefix = ".", tol = 1.0e-10):
    """Clean a MultiPolygon

    This function cleans a MultiPolygon, made up of Polygons (with an exterior
    and any number of interiors), by removing bad points.

    Parameters
    ----------
    multipoly : shapely.geometry.multipolygon.MultiPolygon
        the MultiPolygon
    debug : bool, optional
        print debug messages
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    cleans : shapely.geometry.multipolygon.MultiPolygon
        the cleaned MultiPolygon

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
    from .clean_Polygon import clean_Polygon

    # Check argument ...
    if not isinstance(multipoly, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly\" is not a MultiPolygon") from None

    # Initialize list ...
    polys = []

    # Loop over Polygons ...
    for poly in multipoly.geoms:
        # Append cleaned Polygon to list ...
        polys.append(
            clean_Polygon(
                poly,
                 debug = debug,
                prefix = prefix,
                   tol = tol,
            )
        )

    # Convert list of Polygons to a (unified) MultiPolygon ...
    cleans = shapely.ops.unary_union(polys)
    if debug:
        check(cleans, prefix = prefix)

    # Clean up ...
    del polys

    # Return answer ...
    return cleans
