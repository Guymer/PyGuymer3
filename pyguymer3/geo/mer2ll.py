#!/usr/bin/env python3

# Define function ...
def mer2ll(
    shape1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
       tol = 1.0e-10,
):
    """Transform from Mercator fractions to Longitudes/Latitudes

    This function reads in a shape whose coordinates are fractions on the
    Mercator projection and returns a shape whose coordinates are
    Longitudes/Latitudes.

    Parameters
    ----------
    shape1 : shapely.coords.CoordinateSequence, shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the shape
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    shape2 : shapely.coords.CoordinateSequence, shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the transformed shape

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

    # Import sub-functions ...
    from .mer2llSrc import mer2ll_LinearRing
    from .mer2llSrc import mer2ll_LineString
    from .mer2llSrc import mer2ll_MultiLineString
    from .mer2llSrc import mer2ll_MultiPoint
    from .mer2llSrc import mer2ll_MultiPolygon
    from .mer2llSrc import mer2ll_Point
    from .mer2llSrc import mer2ll_Polygon

    # **************************************************************************

    # Check if it is a Point and return it transformed ...
    if isinstance(shape1, shapely.geometry.point.Point):
        return mer2ll_Point(
            shape1,
             debug = debug,
            prefix = prefix,
        )

    # Check if it is a MultiPoint and return it transformed ...
    if isinstance(shape1, shapely.geometry.multipoint.MultiPoint):
        return mer2ll_MultiPoint(
            shape1,
             debug = debug,
            prefix = prefix,
        )

    # Check if it is a LinearRing and return it transformed ...
    if isinstance(shape1, shapely.geometry.polygon.LinearRing):
        return mer2ll_LinearRing(
            shape1,
             debug = debug,
            prefix = prefix,
        )

    # Check if it is a LineString and return it transformed ...
    if isinstance(shape1, shapely.geometry.linestring.LineString):
        return mer2ll_LineString(
            shape1,
             debug = debug,
            prefix = prefix,
        )

    # Check if it is a MultiLineString and return it transformed ...
    if isinstance(shape1, shapely.geometry.multilinestring.MultiLineString):
        return mer2ll_MultiLineString(
            shape1,
             debug = debug,
            prefix = prefix,
        )

    # Check if it is a Polygon and return it transformed ...
    if isinstance(shape1, shapely.geometry.polygon.Polygon):
        return mer2ll_Polygon(
            shape1,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

    # Check if it is a MultiPolygon and return it transformed ...
    if isinstance(shape1, shapely.geometry.multipolygon.MultiPolygon):
        return mer2ll_MultiPolygon(
            shape1,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

    # Crash ...
    raise TypeError(f"\"shape1\" is an unexpected type ({repr(type(shape1))})") from None
