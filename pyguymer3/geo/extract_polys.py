def extract_polys(shape):
    """Extract the Polygons from the shape

    This function accepts any Shapely geometry and returns a flat list of all of
    the Polygons contained within.

    Parameters
    ----------
    shape :
            the Shapely geometry

    Returns
    -------
    polys : list of shapely.geometry.polygon.Polygon
            a flat list of all of the Polygons

    Note
    ----
    To pass GeoJSON objects you must first convert them to Shapely objects by
    doing something like "shape = shapely.geometry.shape(shape)".
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # **************************************************************************

    # Check type ...
    if shape is None:
        return []

    # **************************************************************************

    # Check type ...
    if isinstance(shape, shapely.geometry.point.Point):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multipoint.MultiPoint):
        return []

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
        if shape.is_empty:
            return []
        return [shape]

    # Check type ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        # Initialize list ...
        polys = []

        # Loop over Polygons ...
        for poly in shape.geoms:
            # Skip if the Polygon is empty ...
            if poly.is_empty:
                continue

            # Append Polygon to list ...
            polys.append(poly)

        # Return answer ...
        return polys

    # Check type ...
    if isinstance(shape, shapely.geometry.collection.GeometryCollection):
        # Initialize list ...
        polys = []

        # Loop over geometries ...
        for geom in shape.geoms:
            # Add lists together ...
            polys += extract_polys(geom)

        # Return answer ...
        return polys

    # **************************************************************************

    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
