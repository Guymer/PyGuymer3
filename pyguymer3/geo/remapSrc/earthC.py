def earthC(shape):
    """Re-map Polygon from Earth-C to Earth-D

    This function finds the extent of the input Polygon that exists on Earth-C
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
    from .._debug import _debug
    from ..extract_polys import extract_polys

    # Check argument ...
    if not isinstance(shape, shapely.geometry.polygon.Polygon):
        raise TypeError("\"shape\" is not a Polygon") from None
    if not shape.is_valid:
        _debug(shape)
        raise Exception(f"\"shape\" is not a valid Polygon ({shapely.validation.explain_validity(shape)})") from None
    if shape.is_empty:
        raise Exception("\"shape\" is an empty Polygon") from None

    # Define Earth-C (as a correctly oriented Polygon) ...
    earth = shapely.geometry.polygon.Polygon(
        [
            (-540.0,  +90.0),
            (-540.0,  -90.0),
            (-180.0,  -90.0),
            (-180.0,  +90.0),
            (-540.0,  +90.0),
        ]
    )
    if not isinstance(earth, shapely.geometry.polygon.Polygon):
        raise Exception("\"earth\" is not a Polygon") from None
    if not earth.is_valid:
        _debug(earth)
        raise Exception(f"\"earth\" is not a valid Polygon ({shapely.validation.explain_validity(earth)})") from None
    if earth.is_empty:
        raise Exception("\"earth\" is an empty Polygon") from None

    # Find the intersection of the Polygon with Earth-C as a list of Polygons ...
    polys = extract_polys(earth.intersection(shape))

    # Initialize list ...
    buffs = []

    # Loop over Polygons ...
    for poly in polys:
        # Check the Polygon ...
        if not isinstance(poly, shapely.geometry.polygon.Polygon):
            raise Exception("\"poly\" is not a Polygon") from None
        if not poly.is_valid:
            _debug(poly)
            raise Exception(f"\"poly\" is not a valid Polygon ({shapely.validation.explain_validity(poly)})") from None
        if poly.is_empty:
            continue

        # Re-map the Polygon of the intersection from Earth-C to Earth-D ...
        buff = shapely.affinity.translate(poly, xoff = +360.0)
        if not isinstance(buff, shapely.geometry.polygon.Polygon):
            raise Exception("\"buff\" is not a Polygon") from None
        if not buff.is_valid:
            _debug(buff)
            raise Exception(f"\"buff\" is not a valid Polygon ({shapely.validation.explain_validity(buff)})") from None
        if buff.is_empty:
            raise Exception("\"buff\" is an empty Polygon") from None

        # Append Polygon to list ...
        buffs.append(buff)

    # Clean up ...
    del polys

    # Return answer ...
    return buffs
