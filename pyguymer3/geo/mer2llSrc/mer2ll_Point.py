#!/usr/bin/env python3

# Define function ...
def mer2ll_Point(
    point1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
):
    """Transform a Point from Mercator fractions to Longitudes/Latitudes

    This function reads in a Point whose coordinates are fractions on the
    Mercator projection and returns a Point whose coordinates are
    Longitudes/Latitudes.

    Parameters
    ----------
    point1 : shapely.geometry.point.Point
        the Point
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs

    Returns
    -------
    point2 : shapely.geometry.point.Point
        the transformed Point

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
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

    # **************************************************************************

    # Check argument ...
    assert isinstance(point1, shapely.geometry.point.Point), "\"point1\" is not a Point"
    if debug:
        check(point1, prefix = prefix)

    # Convert the Point to a NumPy array ...
    points = numpy.array(point1.coords)

    # Project from Longitudes/Latitudes to Mercator fractions in place
    # (including elevation) ...
    points[:, 0] = (points[:, 0] * 360.0) - 180.0                               # [°]
    points[:, 1] = numpy.degrees(numpy.arctan(numpy.sinh(numpy.pi * (1.0 - 2.0 * points[:, 1])))) # [°]

    # Convert array of points to a Point (ignoring elevation) ...
    point2 = shapely.geometry.point.Point(points[0, 0], points[0, 1])
    if debug:
        check(point2, prefix = prefix)

    # Clean up ...
    del points

    # Return answer ...
    return point2
