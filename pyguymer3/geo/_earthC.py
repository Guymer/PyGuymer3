def _earthC(shape):
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

    # Define Earth-C ...
    earthC = shapely.geometry.polygon.Polygon(
        [
            (-540.0,  +90.0),
            (-180.0,  +90.0),
            (-180.0,  -90.0),
            (-540.0,  -90.0),
            (-540.0,  +90.0),
        ]
    )
    if not isinstance(earthC, shapely.geometry.polygon.Polygon):
        raise Exception("\"earthC\" is not a Polygon") from None
    if not earthC.is_valid:
        raise Exception(f"\"earthC\" is not a valid Polygon ({shapely.validation.explain_validity(earthC)})") from None
    if earthC.is_empty:
        raise Exception("\"earthC\" is an empty Polygon") from None

    # Find the intersection of the Polygon with Earth-C as a list of Polygons ...
    polys = _polys(earthC.intersection(shape))

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

        # Re-map the Polygon of the intersection from Earth-C to Earth-D ...
        buff = shapely.affinity.translate(poly, xoff = +360.0)
        if not isinstance(buff, shapely.geometry.polygon.Polygon):
            raise Exception("\"buff\" is not a Polygon") from None
        if not buff.is_valid:
            raise Exception(f"\"buff\" is not a valid Polygon ({shapely.validation.explain_validity(buff)})") from None
        if buff.is_empty:
            raise Exception("\"buff\" is an empty Polygon") from None

        # Append Polygon to list ...
        buffs.append(buff)

    # Return answer ...
    return buffs