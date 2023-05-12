#!/usr/bin/env python3

# Define function ...
def ll2en_LineString(line1, /, *, debug = False):
    """Transform a LineString from Longitudes/Latitudes to Eastings/Northings

    This function reads in a LineString whose coordinates are
    Longitudes/Latitudes and returns a LineString whose coordinates are
    Eastings/Northings on the Ordnance Survey National Grid.

    Parameters
    ----------
    line1 : shapely.geometry.linestring.LineString
        the LineString
    debug : bool, optional
        print debug messages

    Returns
    -------
    line2 : shapely.geometry.linestring.LineString
        the transformed LineString

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
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ..check import check

    # Check argument ...
    if not isinstance(line1, shapely.geometry.linestring.LineString):
        raise TypeError("\"line1\" is not a LineString") from None
    if debug:
        check(line1)

    # Convert the LineString to a NumPy array ...
    points1 = numpy.array(line1.coords)                                         # [m]

    # Project from Eastings/Northings to Longitudes/Latitudes (and elevation) ...
    points2 = cartopy.crs.OSGB().transform_points(cartopy.crs.Geodetic(), points1[:, 0], points1[:, 1]) # [°]

    # Clean up ...
    del points1

    # Convert array of points to a LineString (ignoring elevation) ...
    line2 = shapely.geometry.linestring.LineString(points2[:, :2])
    if debug:
        check(line2)

    # Clean up ...
    del points2

    # Return answer ...
    return line2
