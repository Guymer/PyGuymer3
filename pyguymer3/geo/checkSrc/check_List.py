#!/usr/bin/env python3

# Define function ...
def check_List(
    shape,
    /,
    *,
    prefix = ".",
):
    """Check a list of shapes

    This function checks if a list of shapes is valid.

    Parameters
    ----------
    shape : list
        the list of shapes
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
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .check_CoordinateSequence import check_CoordinateSequence
    from .check_GeometryCollection import check_GeometryCollection
    from .check_LinearRing import check_LinearRing
    from .check_LineString import check_LineString
    from .check_MultiLineString import check_MultiLineString
    from .check_MultiPoint import check_MultiPoint
    from .check_MultiPolygon import check_MultiPolygon
    from .check_Point import check_Point
    from .check_Polygon import check_Polygon

    # **************************************************************************

    # Check argument ...
    assert isinstance(shape, list), "\"shape\" is not a list"

    # Check members ...
    for geom in shape:
        match geom:
            case shapely.coords.CoordinateSequence():
                check_CoordinateSequence(
                    geom,
                )
            case shapely.geometry.point.Point():
                check_Point(
                    geom,
                    prefix = prefix,
                )
            case shapely.geometry.multipoint.MultiPoint():
                check_MultiPoint(
                    geom,
                    prefix = prefix,
                )
            case shapely.geometry.polygon.LinearRing():
                check_LinearRing(
                    geom,
                    prefix = prefix,
                )
            case shapely.geometry.linestring.LineString():
                check_LineString(
                    geom,
                    prefix = prefix,
                )
            case shapely.geometry.multilinestring.MultiLineString():
                check_MultiLineString(
                    geom,
                    prefix = prefix,
                )
            case shapely.geometry.polygon.Polygon():
                check_Polygon(
                    geom,
                    prefix = prefix,
                )
            case shapely.geometry.multipolygon.MultiPolygon():
                check_MultiPolygon(
                    geom,
                    prefix = prefix,
                )
            case shapely.geometry.collection.GeometryCollection():
                check_GeometryCollection(
                    geom,
                    prefix = prefix,
                )
            case _:
                raise TypeError(f"\"geom\" is an unexpected type ({repr(type(geom))})") from None

    # Return answer ...
    return True
