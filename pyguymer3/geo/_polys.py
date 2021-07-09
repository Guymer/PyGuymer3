def _polys(shape):
    # Import special modules ...
    try:
        import shapely
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Check type ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return [shape]

    # Initialize list ...
    polys = []

    # Check type ...
    if isinstance(shape, shapely.geometry.collection.GeometryCollection):
        # Loop over geometries ...
        for geom in shape:
            # Check type ...
            if isinstance(geom, shapely.geometry.linestring.LineString):
                continue

            # Check type ...
            if isinstance(geom, shapely.geometry.polygon.Polygon):
                # Append to list ...
                polys.append(geom)
            else:
                raise TypeError(f"\"geom\" is an unexpected type ({repr(type(geom))})") from None
    else:
        raise Exception(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None

    # Return answer ...
    return polys
