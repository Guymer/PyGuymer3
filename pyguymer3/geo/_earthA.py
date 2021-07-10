def _earthA(shape):
    """Re-map Polygon from Earth-A to Earth-D

    This function finds the extent of the input Polygon that exists on Earth-A
    and re-maps it to Earth-D.

    Parameters
    ----------
    shape : shapely.geometry.polygon.Polygon
            the Polygon

    Returns
    -------
    buffs : list of shapely.geometry.polygon.Polygon
            the list of Polygons that now exist on Earth-D

    Notes
    -----
    According to the Shapely documentation for the function shapely.geometry.polygon.orient():

        "A sign of 1.0 means that the coordinates of the product’s exterior ring will be oriented counter-clockwise."
    """

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

    # Define Earth-A ...
    earthA = shapely.geometry.polygon.Polygon(
        [
            (-360.0, +270.0),
            (   0.0, +270.0),
            (   0.0,  +90.0),
            (-360.0,  +90.0),
            (-360.0, +270.0),
        ]
    )
    if not isinstance(earthA, shapely.geometry.polygon.Polygon):
        raise Exception("\"earthA\" is not a Polygon") from None
    if not earthA.is_valid:
        raise Exception(f"\"earthA\" is not a valid Polygon ({shapely.validation.explain_validity(earthA)})") from None
    if earthA.is_empty:
        raise Exception("\"earthA\" is an empty Polygon") from None

    # Find the intersection of the Polygon with Earth-A as a list of Polygons ...
    polys = _polys(earthA.intersection(shape))

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

        # Re-map the Polygon of the intersection from Earth-A to Earth-D ...
        buff = shapely.affinity.scale(shapely.affinity.translate(poly, xoff = +180.0, yoff = -180.0), xfact = -1.0, yfact = -1.0, origin = (0.0, 0.0))
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
