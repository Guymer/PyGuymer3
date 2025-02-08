#!/usr/bin/env python3

# Define function ...
def zoom(
    lat_deg,
    res,
    /,
    *,
    scale = 1,
):
    """
    Calculate the required zoom to achieve a given resolution in the centre of a tile at a given latitude.

    Parameters
    ----------
    lat_deg : float
        the latitude (in degrees)
    res : float
        the resolution (in metres/pixel)
    scale : int, optional
        the scale of the tile

    Returns
    -------
    zoomOfEarth : int
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
    from ..consts import CIRCUMFERENCE_OF_EARTH

    # Create short-hand ...
    tileSize = scale * 256                                                      # [px]

    # Calculate zoom ...
    lat_rad = math.radians(lat_deg)                                             # [rad]
    zoomOfEarth = math.log2(CIRCUMFERENCE_OF_EARTH * math.cos(lat_rad) / (float(tileSize) * res))

    # Return answer ...
    return round(math.ceil(zoomOfEarth))
