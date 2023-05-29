#!/usr/bin/env python3

# Define function ...
def extract_polys(shape, /, *, onlyValid = False, repair = False):
    """Extract the Polygons from the shape

    This function accepts any Shapely geometry and returns a flat list of all of
    the Polygons contained within.

    Parameters
    ----------
    shape : list, shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon, shapely.geometry.collection.GeometryCollection
        the Shapely geometry
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons

    Returns
    -------
    polys : list of shapely.geometry.polygon.Polygon
        a flat list of all of the Polygons

    Note
    ----
    To pass GeoJSON objects you must first convert them to Shapely objects by
    doing something like ``shape = shapely.geometry.shape(shape)``.

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

    # **************************************************************************

    # Check type ...
    if shape is None:
        return []

    # **************************************************************************

    # Check type ...
    if isinstance(shape, list):
        # Initialize list ...
        polys = []

        # Loop over items ...
        for item in shape:
            # Add lists together ...
            polys += extract_polys(item, onlyValid = onlyValid, repair = repair)

        # Return answer ...
        return polys

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
        # Just return the answer if the user doesn't want any checks or fixes ...
        if not onlyValid:
            # Skip bad Polygons ...
            if shape.is_empty:
                return []

            # Return answer ...
            return [shape]

        # Check if it is valid ...
        if shape.is_valid:
            # Skip bad Polygons ...
            if shape.is_empty:
                return []

            # Return answer ...
            return [shape]

        # Check if the user wants to attempt to fix it ...
        if repair:
            # Try to repair it ...
            shape2 = shape.buffer(0.0)

            # Check if it is valid ...
            if shape2.is_valid:
                # Skip bad Polygons ...
                if shape2.is_empty:
                    return []

                # Return answer ...
                return [shape2]

            # Return answer ...
            return []

        # Return answer ...
        return []

    # Check type ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        # Initialize list ...
        polys = []

        # Loop over Polygons ...
        for poly in shape.geoms:
            # Add lists together ...
            polys += extract_polys(poly, onlyValid = onlyValid, repair = repair)

        # Return answer ...
        return polys

    # Check type ...
    if isinstance(shape, shapely.geometry.collection.GeometryCollection):
        # Initialize list ...
        polys = []

        # Loop over geometries ...
        for geom in shape.geoms:
            # Add lists together ...
            polys += extract_polys(geom, onlyValid = onlyValid, repair = repair)

        # Return answer ...
        return polys

    # **************************************************************************

    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
