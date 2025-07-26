#!/usr/bin/env python3

# Define function ...
def en2ll_LineString(
    line1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
):
    """Transform a LineString from Eastings/Northings to Longitudes/Latitudes

    This function reads in a LineString whose coordinates are Eastings/Northings
    on the Ordnance Survey National Grid and returns a LineString whose
    coordinates are Longitudes/Latitudes.

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

    # Import standard modules ...
    import os
    import pathlib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy_cache").expanduser(),
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
    assert isinstance(line1, shapely.geometry.linestring.LineString), "\"line1\" is not a LineString"
    if debug:
        check(line1, prefix = prefix)

    # Convert the LineString to a NumPy array ...
    points1 = numpy.array(line1.coords)                                         # [m]

    # Project from Eastings/Northings to Longitudes/Latitudes ...
    points2 = cartopy.crs.Geodetic().transform_points(cartopy.crs.OSGB(), points1[:, 0], points1[:, 1]) # [°]

    # Clean up ...
    del points1

    # Convert array of points to a LineString (ignoring elevation) ...
    line2 = shapely.geometry.linestring.LineString(points2[:, :2])
    if debug:
        check(line2, prefix = prefix)

    # Clean up ...
    del points2

    # Return answer ...
    return line2
