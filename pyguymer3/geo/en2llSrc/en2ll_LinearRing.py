#!/usr/bin/env python3

# Define function ...
def en2ll_LinearRing(ring1, /, *, debug = False):
    """Transform a LinearRing from Eastings/Northings to Longitudes/Latitudes

    This function reads in a LinearRing whose coordinates are Eastings/Northings
    on the Ordnance Survey National Grid and returns a LinearRing whose
    coordinates are Longitudes/Latitudes.

    Parameters
    ----------
    ring1 : shapely.geometry.polygon.LinearRing
        the LinearRing
    debug : bool, optional
        print debug messages

    Returns
    -------
    ring2 : shapely.geometry.polygon.LinearRing
        the transformed LinearRing

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
    if not isinstance(ring1, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"ring1\" is not a LinearRing") from None
    if debug:
        check(ring1)

    # Convert the LinearRing to a NumPy array ...
    points1 = numpy.array(ring1.coords)                                         # [m]

    # Project from Eastings/Northings to Longitudes/Latitudes (and elevation) ...
    points2 = cartopy.crs.Geodetic().transform_points(cartopy.crs.OSGB(), points1[:, 0], points1[:, 1]) # [°]

    # Clean up ...
    del points1

    # Convert array of points to a LinearRing (ignoring elevation) ...
    ring2 = shapely.geometry.polygon.LinearRing(points2[:, :2])
    if debug:
        check(ring2)

    # Clean up ...
    del points2

    # Return answer ...
    return ring2
