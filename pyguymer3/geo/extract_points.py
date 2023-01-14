#!/usr/bin/env python3

# Define function ...
def extract_points(shape, kwArgCheck = None, onlyValid = False):
    """Extract the Points from the shape

    This function accepts any Shapely geometry and returns a flat list of all of
    the Points contained within.

    Parameters
    ----------
    shape :
        the Shapely geometry
    onlyValid : bool, optional
        only return valid Points (checks for validity can take a while, if
        being called often)

    Returns
    -------
    points : list of shapely.geometry.point.Point
        a flat list of all of the Points

    Notes
    -----
    To pass GeoJSON objects you must first convert them to Shapely objects by
    doing something like "shape = shapely.geometry.shape(shape)".

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
        points = []

        # Loop over items ...
        for item in shape:
            # Add lists together ...
            points += extract_points(item, onlyValid = onlyValid)

        # Return answer ...
        return points

    # Check type ...
    if isinstance(shape, shapely.geometry.point.Point):
        # Just return the answer if the user doesn't want any checks ...
        if not onlyValid:
            # Skip bad Points ...
            if shape.is_empty:
                return []

            # Return answer ...
            return [shape]

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
            points += extract_points(point, onlyValid = onlyValid)

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
            points += extract_points(geom, onlyValid = onlyValid)

        # Return answer ...
        return points

    # **************************************************************************

    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
