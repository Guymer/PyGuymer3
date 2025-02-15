#!/usr/bin/env python3

# Define function ...
def mer2ll_LinearRing(
    ring1,
    /,
    *,
     debug = __debug__,
    prefix = ".",
):
    """Transform a LinearRing from Mercator fractions to Longitudes/Latitudes

    This function reads in a LinearRing whose coordinates are fractions on the
    Mercator projection and returns a LinearRing whose coordinates are
    Longitudes/Latitudes.

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
    assert isinstance(ring1, shapely.geometry.polygon.LinearRing), "\"ring1\" is not a LinearRing"
    if debug:
        check(ring1, prefix = prefix)

    # Convert the Point to a NumPy array ...
    points = numpy.array(ring1.coords)

    # Project from Longitudes/Latitudes to Mercator fractions in place
    # (including elevation) ...
    points[:, 0] = (points[:, 0] * 360.0) - 180.0                               # [°]
    points[:, 1] = numpy.degrees(numpy.atan(numpy.sinh(numpy.pi * (1.0 - 2.0 * points[:, 1])))) # [°]

    # Convert array of points to a LinearRing (ignoring elevation) ...
    ring2 = shapely.geometry.polygon.LinearRing(points[:, :2])
    if debug:
        check(ring2, prefix = prefix)

    # Clean up ...
    del points

    # Return answer ...
    return ring2
