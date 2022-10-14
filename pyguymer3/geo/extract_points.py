def extract_points(shape):
    """Extract the Points from the shape

    This function accepts any Shapely geometry and returns a flat list of all of
    the Points contained within.

    Parameters
    ----------
    shape :
        the Shapely geometry

    Returns
    -------
    points : list of shapely.geometry.point.Point
        a flat list of all of the Points

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

    # Check type ...
    if shape is None:
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.point.Point):
        # Check if it is valid ...
        if shape.is_valid:
            # Skip bad Points ...
            if shape.is_empty:
                return []

            # Return answer ...
            return [shape]

        # Return answer ...
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multipoint.MultiPoint):
        # Initialize list ...
        points = []

        # Loop over Points ...
        for point in shape.geoms:
            # Add lists together ...
            points += extract_points(point)

        # Return answer ...
        return points

    # Check type ...
    if isinstance(shape, shapely.geometry.polygon.LinearRing):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.collection.GeometryCollection):
        # Initialize list ...
        points = []

        # Loop over geometries ...
        for geom in shape.geoms:
            # Add lists together ...
            points += extract_points(geom)

        # Return answer ...
        return points

    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
