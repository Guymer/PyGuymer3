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
    if isinstance(shape, shapely.geometry.multipoint.MultiPoint):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return [shape]

    # Initialize list ...
    polys = []

    # Check type ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        # Loop over Polygons ...
        for poly in shape:
            # Check type ...
            if isinstance(poly, shapely.geometry.polygon.Polygon):
                # Append to list ...
                polys.append(poly)
            else:
                raise TypeError(f"\"poly\" is an unexpected type ({repr(type(poly))})") from None
    elif isinstance(shape, shapely.geometry.collection.GeometryCollection):
        # Loop over geometries ...
        for geom in shape:
            # Check type ...
            if isinstance(geom, shapely.geometry.linestring.LineString):
                continue

            # Check type ...
            if isinstance(geom, shapely.geometry.multilinestring.MultiLineString):
                continue

            # Check type ...
            if isinstance(geom, shapely.geometry.polygon.Polygon):
                # Append to list ...
                polys.append(geom)
            elif isinstance(geom, shapely.geometry.multipolygon.MultiPolygon):
                # Loop over Polygons ...
                for poly in geom:
                    # Check type ...
                    if isinstance(poly, shapely.geometry.polygon.Polygon):
                        # Append to list ...
                        polys.append(poly)
                    else:
                        raise TypeError(f"\"poly\" is an unexpected type ({repr(type(poly))})") from None
            else:
                raise TypeError(f"\"geom\" is an unexpected type ({repr(type(geom))})") from None
    else:
        raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None

    # Return answer ...
    return polys
