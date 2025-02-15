#!/usr/bin/env python3

# Define function ...
def mer2ll_LineString(
    line1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
):
    """Transform a LineString from Mercator fractions to Longitudes/Latitudes

    This function reads in a LineString whose coordinates are fractions on the
    Mercator projection and returns a LineString whose coordinates are
    Longitudes/Latitudes.

    Parameters
    ----------
    line1 : shapely.geometry.linestring.LineString
        the LineString
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs

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
    assert isinstance(line1, shapely.geometry.linestring.LineString), "\"line1\" is not a LineString"
    if debug:
        check(line1, prefix = prefix)

    # Convert the Point to a NumPy array ...
    points = numpy.array(line1.coords)

    # Project from Longitudes/Latitudes to Mercator fractions in place
    # (including elevation) ...
    points[:, 0] = (points[:, 0] * 360.0) - 180.0                               # [°]
    points[:, 1] = numpy.degrees(numpy.atan(numpy.sinh(numpy.pi * (1.0 - 2.0 * points[:, 1])))) # [°]

    # Convert array of points to a LineString (ignoring elevation) ...
    line2 = shapely.geometry.linestring.LineString(points[:, :2])
    if debug:
        check(line2, prefix = prefix)

    # Clean up ...
    del points

    # Return answer ...
    return line2
