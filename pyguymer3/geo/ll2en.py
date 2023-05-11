#!/usr/bin/env python3

# Define function ...
def ll2en(shape1, /, *, debug = False):
    """Transform from Longitudes/Latitudes to Eastings/Northings

    This function reads in a shape whose coordinates are Longitudes/Latitudes
    and returns a shape whose coordinates are Eastings/Northings on the Ordnance
    Survey National Grid.

    Parameters
    ----------
    shape1 : shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the shape
    debug : bool, optional
        print debug messages

    Returns
    -------
    shape2 : shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
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
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None

    # Import sub-functions ...
    from .check import check

    # Check argument ...
    if debug:
        check(shape1)

    # Project from Longitudes/Latitudes to Eastings/Northings ...
    shape2 = cartopy.crs.OSGB().project_geometry(shape1, src_crs = cartopy.crs.Geodetic())
    if debug:
        check(shape2)

    # Return answer ...
    return shape2
