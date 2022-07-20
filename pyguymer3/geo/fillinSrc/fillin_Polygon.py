def fillin_Polygon(poly, fill, kwArgCheck = None, debug = False, fillSpace = "EuclideanSpace"):
    """Fill in a Polygon

    This function reads in a Polygon (with an exterior and any number of
    interiors) that exists on the surface of the Earth and returns a
    Polygon of the same Polygon filled in by a constant distance: either in
    degrees in Euclidean space; or in metres in Geodesic space.

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
            the Polygon
    fill : float
            the Euclidean or Geodesic distance to fill in between each point within the shape by (in degrees or metres)
    debug : bool, optional
            print debug messages
    fillSpace : str, optional
            the geometric space to perform the filling in (either "EuclideanSpace" or "GeodesicSpace")

    Returns
    -------
    fills : shapely.geometry.polygon.Polygon
            the filled in Polygon
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
    from .fillin_LinearRing import fillin_LinearRing

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon") from None
    check(poly)

    # Filled in exterior LinearRing ...
    exterior = fillin_LinearRing(poly.exterior, fill, debug = debug, fillSpace = fillSpace)

    # Initialize list ...
    interiors = []

    # Loop over interior LinearRings ...
    for interior in poly.interiors:
        # Skip if it doesn't contain any area ...
        if interior.area <= 0.0:
            if debug:
                print(f"INFO: Removing a zero-area interior at ({interior.centroid.x:.6f}°,{interior.centroid.y:.6f}°).")
            continue

        # Append filled in interior LinearRing to list ...
        interiors.append(fillin_LinearRing(interior, fill, debug = debug, fillSpace = fillSpace))

    # Convert exterior LinearRing and list of interior LinearRings to a
    # correctly oriented Polygon ...
    fills = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(exterior, interiors))
    check(fills)

    # Clean up ...
    del exterior, interiors

    # Return answer ...
    return fills
