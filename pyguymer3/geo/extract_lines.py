def extract_lines(shape, kwArgCheck = None, keepInvalid = False):
    """Extract the LineStrings from the shape

    This function accepts any Shapely geometry and returns a flat list of all of
    the LineStrings contained within.

    Parameters
    ----------
    shape :
        the Shapely geometry

    Returns
    -------
    lines : list of shapely.geometry.linestring.LineString
        a flat list of all of the LineStrings

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
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Check type ...
    if shape is None:
        return []

    # **************************************************************************

    # Check type ...
    if isinstance(shape, list):
        # Initialize list ...
        lines = []

        # Loop over items ...
        for item in shape:
            # Add lists together ...
            lines += extract_lines(item, keepInvalid = keepInvalid)

        # Return answer ...
        return lines

    # Check type ...
    if isinstance(shape, shapely.geometry.point.Point):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multipoint.MultiPoint):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.polygon.LinearRing):
        # Just return the answer if the user doesn't want any checks or fixes ...
        if keepInvalid:
            return [shape]

        # Check if it is valid ...
        if shape.is_valid:
            # Skip bad LineStrings ...
            if shape.is_empty:
                return []

            # Return answer ...
            return [shape]

        # Return answer ...
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        # Just return the answer if the user doesn't want any checks or fixes ...
        if keepInvalid:
            return [shape]

        # Check if it is valid ...
        if shape.is_valid:
            # Skip bad LineStrings ...
            if shape.is_empty:
                return []

            # Return answer ...
            return [shape]

        # Return answer ...
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        # Initialize list ...
        lines = []

        # Loop over LineStrings ...
        for line in shape.geoms:
            # Add lists together ...
            lines += extract_lines(line, keepInvalid = keepInvalid)

        # Return answer ...
        return lines

    # Check type ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.collection.GeometryCollection):
        # Initialize list ...
        lines = []

        # Loop over geometries ...
        for geom in shape.geoms:
            # Add lists together ...
            lines += extract_lines(geom, keepInvalid = keepInvalid)

        # Return answer ...
        return lines

    # **************************************************************************

    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
