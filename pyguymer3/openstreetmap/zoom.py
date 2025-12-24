#!/usr/bin/env python3

# Define function ...
def zoom(
    lat_deg,
    res,
    /,
    *,
     ceil = True,
    floor = False,
    scale = 1,
):
    """
    Calculate the required zoom to achieve a given resolution in the centre of a
    tile at a given latitude. If both ``ceil`` and ``floor`` are ``False`` then
    return the floating-point answer instead of an integer.

    Parameters
    ----------
    lat_deg : float
        the latitude (in degrees)
    res : float
        the resolution (in metres/pixel)
    ceil : bool, optional
        convert the floating-point answer to an integer using ``math.ceil()``
    floor : bool, optional
        convert the floating-point answer to an integer using ``math.floor()``
    scale : int, optional
        the scale of the tile

    Returns
    -------
    zoomOfEarth : int or float
        the zoom

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
    from .._consts import CIRCUMFERENCE_OF_EARTH

    # Create short-hand ...
    tileSize = scale * 256                                                      # [px]

    # Calculate zoom ...
    lat_rad = math.radians(lat_deg)                                             # [rad]
    zoomOfEarth = math.log2(CIRCUMFERENCE_OF_EARTH * math.cos(lat_rad) / (float(tileSize) * res))

    # Return answer ...
    if ceil:
        return round(math.ceil(zoomOfEarth))
    if floor:
        return round(math.floor(zoomOfEarth))
    return zoomOfEarth
