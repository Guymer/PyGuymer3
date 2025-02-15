#!/usr/bin/env python3

# Define function ...
def ll2mer_Polygon(
    poly1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
       tol = 1.0e-10,
):
    """Transform a Polygon from Longitudes/Latitudes to Mercator fractions

    This function reads in a Polygon whose coordinates are Longitudes/Latitudes
    and returns a Polygon whose coordinates are fractions on the Mercator projection.

    Parameters
    ----------
    poly1 : shapely.geometry.polygon.Polygon
        the Polygon
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    poly2 : shapely.geometry.polygon.Polygon
        the transformed Polygon

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
    from .ll2mer_LinearRing import ll2mer_LinearRing

    # **************************************************************************

    # Check argument ...
    assert isinstance(poly1, shapely.geometry.polygon.Polygon), "\"poly1\" is not a Polygon"
    if debug:
        check(poly1, prefix = prefix)

    # Transform exterior LinearRing ...
    exterior = ll2mer_LinearRing(
        poly1.exterior,
         debug = debug,
        prefix = prefix,
    )

    # Initialize list ...
    interiors = []

    # Loop over interior LinearRings ...
    for interior in poly1.interiors:
        # Skip if it doesn't contain any length ...
        if interior.length < tol:
            if debug:
                print(f"INFO: Removing a tiny-length interior ring at ({interior.centroid.x:+.6f}m,{interior.centroid.y:+.6f}m).")
            continue

        # Append transformed interior LinearRing to list ...
        interiors.append(
            ll2mer_LinearRing(
                interior,
                 debug = debug,
                prefix = prefix,
            )
        )

    # Convert exterior LinearRing and list of interior LinearRings to a
    # correctly oriented Polygon ...
    poly2 = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(exterior, interiors))
    if debug:
        check(poly2, prefix = prefix)

    # Return answer ...
    return poly2
