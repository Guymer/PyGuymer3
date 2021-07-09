def _earthD(shape):
    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._polys import _polys

    # Check argument ...
    if not isinstance(shape, shapely.geometry.polygon.Polygon):
        raise TypeError("\"shape\" is not a Polygon") from None
    if not shape.is_valid:
        raise Exception(f"\"shape\" is not a valid Polygon ({shapely.validation.explain_validity(shape)})") from None
    if shape.is_empty:
        raise Exception("\"shape\" is an empty Polygon") from None

    # Define Earth-D ...
    earthD = shapely.geometry.polygon.Polygon(
        [
            (-180.0,  +90.0),
            (+180.0,  +90.0),
            (+180.0,  -90.0),
            (-180.0,  -90.0),
            (-180.0,  +90.0),
        ]
    )
    if not isinstance(earthD, shapely.geometry.polygon.Polygon):
        raise Exception("\"earthD\" is not a Polygon") from None
    if not earthD.is_valid:
        raise Exception(f"\"earthD\" is not a valid Polygon ({shapely.validation.explain_validity(earthD)})") from None
    if earthD.is_empty:
        raise Exception("\"earthD\" is an empty Polygon") from None

    # Find the intersection of the Polygon with Earth-D as a list of Polygons ...
    polys = _polys(earthD.intersection(shape))

    # Initialize list ...
    buffs = []

    # Loop over Polygons ...
    for poly in polys:
        # Check the Polygon ...
        if not isinstance(poly, shapely.geometry.polygon.Polygon):
            raise Exception("\"poly\" is not a Polygon") from None
        if not poly.is_valid:
            raise Exception(f"\"poly\" is not a valid Polygon ({shapely.validation.explain_validity(poly)})") from None
        if poly.is_empty:
            continue

        # Append Polygon to list ...
        buffs.append(poly)

    # Return answer ...
    return buffs
