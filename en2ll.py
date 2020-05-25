def en2ll(poly1):
    """
    This function reads in a Polygon whose coordinates are Eastings/Northings on
    the Ordnance Survey National Grid and returns a Polygon whose coordinates
    are Longitudes/Latitudes, or False on error.
    """

    # Import special modules ...
    try:
        import convertbng
        import convertbng.util
    except:
        raise Exception("run \"pip install --user convertbng\"")
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("run \"pip install --user shapely\"")

    # Check argument ...
    if not isinstance(poly1, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly1\" is not a Polygon")
    if not poly1.is_valid:
        return False

    # Initialize lists ...
    exteriorRing = []
    interiorRings = []

    # Loop over exterior ring coordinates ...
    for east, north in poly1.exterior.coords:
        # Convert easting/northing to longitude/latitude and append to list ...
        lon, lat = convertbng.util.convert_lonlat(east, north)                  # [°], [°]
        exteriorRing.append((lon[0], lat[0]))

    # Convert ring to LinearRing ...
    exteriorRing = shapely.geometry.polygon.LinearRing(exteriorRing)
    if not exteriorRing.is_valid:
        return False

    # Loop over interior rings ...
    for interior in poly1.interiors:
        # Initialize list ...
        interiorRing = []

        # Loop over interior ring coordinates ...
        for east, north in interior.coords:
            # Convert easting/northing to longitude/latitude and append to list ...
            lon, lat = convertbng.util.convert_lonlat(east, north)              # [°], [°]
            interiorRing.append((lon[0], lat[0]))

        # Convert ring to LinearRing ...
        interiorRing = shapely.geometry.polygon.LinearRing(interiorRing)
        if not interiorRing.is_valid:
            return False

        # Append LinearRing to list ...
        interiorRings.append(interiorRing)

    # Convert LinearRings to Polygon ...
    poly2 = shapely.geometry.polygon.Polygon(exteriorRing, interiorRings)
    if not poly2.is_valid:
        return False

    # Return answer ...
    return poly2
