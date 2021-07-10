def fillin_Polygon(poly, fill, kwArgCheck = None, debug = False):
    """Fill in a Polygon

    This function reads in a Polygon (with an exterior and any number of
    interiors) that exists on the surface of the Earth and returns a
    Polygon of the same Polygon filled in by a constant distance (in degrees).

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
            the Polygon
    fill : float
            the distance to fill in between each point within the shape by (in degrees)
    debug : bool, optional
            print debug messages

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
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from .fillin_LinearRing import fillin_LinearRing

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon") from None
    if not poly.is_valid:
        raise Exception(f"\"poly\" is not a valid Polygon ({shapely.validation.explain_validity(poly)})") from None
    if poly.is_empty:
        raise Exception("\"poly\" is an empty Polygon") from None

    # Initialize list ...
    interiors = []

    # Loop over interior LinearRings ...
    for ring in poly.interiors:
        # Append filled in interior LinearRing to list ...
        interiors.append(fillin_LinearRing(ring, fill, debug = debug))

    # Convert exterior LinearRing and list of interior LinearRings to a
    # correctly oriented Polygon ...
    fills = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(fillin_LinearRing(poly.exterior, fill, debug = debug), interiors))
    if not isinstance(fills, shapely.geometry.polygon.Polygon):
        raise TypeError("\"fills\" is not a Polygon") from None
    if not fills.is_valid:
        raise Exception(f"\"fills\" is not a valid Polygon ({shapely.validation.explain_validity(fills)})") from None
    if fills.is_empty:
        raise Exception("\"fills\" is an empty Polygon") from None

    # Return answer ...
    return fills
