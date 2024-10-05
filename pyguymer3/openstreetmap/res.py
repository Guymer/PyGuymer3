#!/usr/bin/env python3

# Define function ...
def res(
    lat_deg,
    zoom,
    /,
):
    """
    Calculate the resolution in the centre of a tile at a given latitude and for
    a given zoom.

    Parameters
    ----------
    lat_deg : float
        the latitude (in degrees)
    zoom : int
        the zoom

    Returns
    -------
    resOfEarth : float
        the resolution (in metres/pixel)

    Notes
    -----
    `OpenStreetMap have tabulated a few values <https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale>`_ .

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import math

    # Import sub-functions ...
    from ..consts import CIRCUMFERENCE_OF_EARTH

    lat_rad = math.radians(lat_deg)                                             # [rad]
    n = pow(2, zoom)
    resOfEarth = CIRCUMFERENCE_OF_EARTH * math.cos(lat_rad) / (256.0 * n)       # [m/px]

    # Return answer ...
    return resOfEarth
