def extract_lines(shape):
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
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Check type ...
    if isinstance(shape, shapely.geometry.point.Point):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return [shape]

    # Check type ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        print(type(shape.exterior))
        for ring in shape.interiors:
            print(type(ring))
        raise Exception("not implemented yet") from None

    # Check type ...
    if isinstance(shape, shapely.geometry.multipoint.MultiPoint):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        # Initialize list ...
        lines = []

        # Loop over LineStrings ...
        for line in shape:
            # Append LineString to list ...
            lines.append(line)

        # Return answer ...
        return lines

    # Check type ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        # Initialize list ...
        lines = []

        # Loop over Polygons ...
        for poly in shape:
            # Add lists together ...
            lines += extract_lines(poly)

        # Return answer ...
        return lines

    # Check type ...
    if isinstance(shape, shapely.geometry.collection.GeometryCollection):
        # Initialize list ...
        lines = []

        # Loop over geometries ...
        for geom in shape:
            # Add lists together ...
            lines += extract_lines(geom)

        # Return answer ...
        return lines

    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
