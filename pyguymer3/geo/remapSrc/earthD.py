def earthD(shape):
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

        "A sign of 1.0 means that the coordinates of the product’s exterior ring
        will be oriented counter-clockwise."

    Copyright 2018 Thomas Guymer [1]_

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

    # Define Earth-D (as a correctly oriented Polygon) ...
    earth = shapely.geometry.polygon.Polygon(
        [
            (-180.0,  +90.0),
            (-180.0,  -90.0),
            (+180.0,  -90.0),
            (+180.0,  +90.0),
            (-180.0,  +90.0),
        ]
    )
    if not isinstance(earth, shapely.geometry.polygon.Polygon):
        raise Exception("\"earth\" is not a Polygon") from None
    check(earth)

    # Find the intersection of the Polygon with Earth-D as a list of Polygons ...
    polys = extract_polys(earth.intersection(shape))

    # Initialize list ...
    buffs = []

    # Loop over Polygons ...
    for poly in polys:
        # Check the Polygon ...
        if not isinstance(poly, shapely.geometry.polygon.Polygon):
            raise Exception("\"poly\" is not a Polygon") from None
        check(poly)

        # Append Polygon to list ...
        buffs.append(poly)

    # Clean up ...
    del polys

    # Return answer ...
    return buffs
