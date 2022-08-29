def earthA(shape):
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

        "A sign of 1.0 means that the coordinates of the product’s exterior ring
        will be oriented counter-clockwise."

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
    from ..extract_polys import extract_polys

    # Check argument ...
    if not isinstance(shape, shapely.geometry.polygon.Polygon):
        raise TypeError("\"shape\" is not a Polygon") from None
    check(shape)

    # Define Earth-A (as a correctly oriented Polygon) ...
    earth = shapely.geometry.polygon.Polygon(
        [
            (-360.0, +270.0),
            (-360.0,  +90.0),
            (   0.0,  +90.0),
            (   0.0, +270.0),
            (-360.0, +270.0),
        ]
    )
    if not isinstance(earth, shapely.geometry.polygon.Polygon):
        raise Exception("\"earth\" is not a Polygon") from None
    check(earth)

    # Find the intersection of the Polygon with Earth-A as a list of Polygons ...
    polys = extract_polys(earth.intersection(shape))

    # Initialize list ...
    buffs = []

    # Loop over Polygons ...
    for poly in polys:
        # Check the Polygon ...
        if not isinstance(poly, shapely.geometry.polygon.Polygon):
            raise Exception("\"poly\" is not a Polygon") from None
        check(poly)

        # Re-map the Polygon of the intersection from Earth-A to Earth-D ...
        buff = shapely.affinity.scale(shapely.affinity.translate(poly, xoff = +180.0, yoff = -180.0), xfact = -1.0, yfact = -1.0, origin = (0.0, 0.0))
        if not isinstance(buff, shapely.geometry.polygon.Polygon):
            raise Exception("\"buff\" is not a Polygon") from None
        check(buff)

        # Append Polygon to list ...
        buffs.append(buff)

    # Clean up ...
    del polys

    # Return answer ...
    return buffs
