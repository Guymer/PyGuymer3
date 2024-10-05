#!/usr/bin/env python3

# Define function ...
def en2ll_Point(
    point1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
):
    """Transform a Point from Eastings/Northings to Longitudes/Latitudes

    This function reads in a Point whose coordinates are Eastings/Northings on
    the Ordnance Survey National Grid and returns a Point whose coordinates are
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

    # Import standard modules ...
    import os

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : os.path.expanduser("~/.local/share/cartopy_cache"),
            }
        )
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

    # **************************************************************************

    # Check argument ...
    assert isinstance(point1, shapely.geometry.point.Point), "\"point1\" is not a Point"
    if debug:
        check(point1, prefix = prefix)

    # Convert the Point to a NumPy array ...
    points1 = numpy.array(point1.coords)                                        # [m]

    # Project from Eastings/Northings to Longitudes/Latitudes (and elevation) ...
    points2 = cartopy.crs.Geodetic().transform_points(cartopy.crs.OSGB(), points1[:, 0], points1[:, 1]) # [°]

    # Clean up ...
    del points1

    # Convert array of points to a Point (ignoring elevation) ...
    point2 = shapely.geometry.point.Point(points2[0, 0], points2[0, 1])
    if debug:
        check(point2, prefix = prefix)

    # Clean up ...
    del points2

    # Return answer ...
    return point2
