def en2ll(poly1):
    """
    This function reads in a Polygon whose coordinates are Eastings/Northings on
    the Ordnance Survey National Grid and returns a Polygon whose coordinates
    are Longitudes/Latitudes, or False on error.
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._en2ll import _en2ll
    from .check import check

    # Check argument ...
    if not isinstance(poly1, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly1\" is not a Polygon") from None
    check(poly1)

    # Initialize list ...
    exteriorRing = []

    # Loop over exterior ring coordinates ...
    for east, north in poly1.exterior.coords:
        # Convert easting/northing to longitude/latitude and append to list ...
        exteriorRing.append(_en2ll(east, north))

    # Convert ring to LinearRing ...
    exteriorRing = shapely.geometry.polygon.LinearRing(exteriorRing)
    check(exteriorRing)

    # Initialize list ...
    interiorRings = []

    # Loop over interior rings ...
    for interior in poly1.interiors:
        # Initialize list ...
        interiorRing = []

        # Loop over interior ring coordinates ...
        for east, north in interior.coords:
            # Convert easting/northing to longitude/latitude and append to list ...
            interiorRing.append(_en2ll(east, north))

        # Convert ring to LinearRing ...
        interiorRing = shapely.geometry.polygon.LinearRing(interiorRing)
        check(interiorRing)

        # Append LinearRing to list ...
        interiorRings.append(interiorRing)

        # Clean up ...
        del interiorRing

    # Convert LinearRings to Polygon ...
    poly2 = shapely.geometry.polygon.Polygon(exteriorRing, interiorRings)
    check(poly2)

    # Clean up ...
    del exteriorRing, interiorRings

    # Return answer ...
    return poly2
