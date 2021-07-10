def _earthD(shape):
    """Re-map Polygon from Earth-D to Earth-D

    This function finds the extent of the input Polygon that exists on Earth-D
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
    from .extract_polys import extract_polys

    # Check argument ...
    if not isinstance(shape, shapely.geometry.polygon.Polygon):
        raise TypeError("\"shape\" is not a Polygon") from None
    if not shape.is_valid:
        raise Exception(f"\"shape\" is not a valid Polygon ({shapely.validation.explain_validity(shape)})") from None
    if shape.is_empty:
        raise Exception("\"shape\" is an empty Polygon") from None

    # Define Earth-D (as a correctly oriented Polygon) ...
    earthD = shapely.geometry.polygon.Polygon(
        [
            (-180.0,  +90.0),
            (-180.0,  -90.0),
            (+180.0,  -90.0),
            (+180.0,  +90.0),
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
    polys = extract_polys(earthD.intersection(shape))

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
