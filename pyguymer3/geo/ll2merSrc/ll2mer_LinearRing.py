#!/usr/bin/env python3

# Define function ...
def ll2mer_LinearRing(
    ring1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
):
    """Transform a LinearRing from Longitudes/Latitudes to Mercator fractions

    This function reads in a LinearRing whose coordinates are
    Longitudes/Latitudes and returns a LinearRing whose coordinates are
    fractions on the Mercator projection.

    Parameters
    ----------
    ring1 : shapely.geometry.polygon.LinearRing
        the LinearRing
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs

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
    assert isinstance(ring1, shapely.geometry.polygon.LinearRing), "\"ring1\" is not a LinearRing"
    if debug:
        check(ring1, prefix = prefix)

    # Convert the LinearRing to a NumPy array ...
    points1 = numpy.array(ring1.coords)                                         # [m]

    # Project from Longitudes/Latitudes to Mercator fractions ...
    points2 = cartopy.crs.Mercator().transform_points(cartopy.crs.Geodetic(), points1[:, 0], points1[:, 1]) # [°]

    # Clean up ...
    del points1

    # Convert array of points to a LinearRing (ignoring elevation) ...
    ring2 = shapely.geometry.polygon.LinearRing(points2[:, :2])
    if debug:
        check(ring2, prefix = prefix)

    # Clean up ...
    del points2

    # Return answer ...
    return ring2
