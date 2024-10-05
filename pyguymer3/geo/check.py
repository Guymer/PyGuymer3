#!/usr/bin/env python3

# Define function ...
def check(
    shape,
    /,
    *,
    prefix = ".",
):
    """Check a shape

    This function checks if a shape is valid.

    Parameters
    ----------
    shape : shapely.coords.CoordinateSequence, shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon, shapely.geometry.collection.GeometryCollection
        the shape
    prefix : str, optional
        change the name of the output debugging CSVs

    Notes
    -----
    According to the `Shapely documentation for the function
    shapely.geometry.polygon.orient()
    <https://shapely.readthedocs.io/en/stable/manual.html#shapely.geometry.polygon.orient>`_ :

        "A sign of 1.0 means that the coordinates of the product's exterior ring
        will be oriented counter-clockwise."

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

    # Import sub-functions ...
    from .checkSrc import check_CoordinateSequence
    from .checkSrc import check_GeometryCollection
    from .checkSrc import check_LinearRing
    from .checkSrc import check_LineString
    from .checkSrc import check_MultiLineString
    from .checkSrc import check_MultiPoint
    from .checkSrc import check_MultiPolygon
    from .checkSrc import check_Point
    from .checkSrc import check_Polygon

    # **************************************************************************

    # Check if it is a CoordinateSequence and return it checked ...
    if isinstance(shape, shapely.coords.CoordinateSequence):
        return check_CoordinateSequence(
            shape,
        )

    # Check if it is a Point and return it checked ...
    if isinstance(shape, shapely.geometry.point.Point):
        return check_Point(
            shape,
            prefix = prefix,
        )

    # Check if it is a MultiPoint and return it checked ...
    if isinstance(shape, shapely.geometry.multipoint.MultiPoint):
        return check_MultiPoint(
            shape,
            prefix = prefix,
        )

    # Check if it is a LinearRing and return it checked ...
    if isinstance(shape, shapely.geometry.polygon.LinearRing):
        return check_LinearRing(
            shape,
            prefix = prefix,
        )

    # Check if it is a LineString and return it checked ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return check_LineString(
            shape,
            prefix = prefix,
        )

    # Check if it is a MultiLineString and return it checked ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        return check_MultiLineString(
            shape,
            prefix = prefix,
        )

    # Check if it is a Polygon and return it checked ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return check_Polygon(
            shape,
            prefix = prefix,
        )

    # Check if it is a MultiPolygon and return it checked ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return check_MultiPolygon(
            shape,
            prefix = prefix,
        )

    # Check if it is a GeometryCollection and return it checked ...
    if isinstance(shape, shapely.geometry.collection.GeometryCollection):
        return check_GeometryCollection(
            shape,
            prefix = prefix,
        )

    # Crash ...
    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
