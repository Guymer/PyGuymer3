def clean_Polygon(poly, kwArgCheck = None, debug = False, prefix = ".", tol = 1.0e-10):
    """Clean a Polygon

    This function cleans a Polygon (with an exterior and any number of
    interiors) by removing bad points.

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
        the Polygon
    debug : bool, optional
        print debug messages
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    cleans : shapely.geometry.polygon.Polygon
        the cleaned Polygon

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
    from .clean_LinearRing import clean_LinearRing

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon") from None

    # Clean exterior LinearRing ...
    exterior = clean_LinearRing(
        poly.exterior,
         debug = debug,
        prefix = prefix,
           tol = tol,
    )

    # Initialize list ...
    interiors = []

    # Loop over interior LinearRings ...
    for interior in poly.interiors:
        # Skip if it doesn't contain any length ...
        if interior.length < tol:
            if debug:
                print(f"INFO: Removing a tiny-length interior ring at ({interior.centroid.x:+.6f}°,{interior.centroid.y:+.6f}°).")
            continue

        # Append cleaned interior LinearRing to list ...
        interiors.append(
            clean_LinearRing(
                interior,
                 debug = debug,
                prefix = prefix,
                   tol = tol,
            )
        )

    # Convert exterior LinearRing and list of interior LinearRings to a
    # correctly oriented Polygon ...
    cleans = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(exterior, interiors))
    if debug:
        check(cleans, prefix = prefix)

    # Clean up ...
    del exterior, interiors

    # Return answer ...
    return cleans
