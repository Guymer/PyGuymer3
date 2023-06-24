#!/usr/bin/env python3

# Define function ...
def zoom(lat_deg, res, /):
    """
    Calculate the required zoom to achieve a given resolution in the centre of a tile at a given latitude.

    Parameters
    ----------
    lat_deg : float
        the latitude (in degrees)
    res : float
        the resolution (in metres/pixel)

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

    # Define contants  ...
    # NOTE: See https://en.wikipedia.org/wiki/Earth_radius#Mean_radius
    # NOTE: Using a radius of 6,371,008.8 m equates to:
    #         * 1 °  = 111.195 km
    #         * 1 m° = 111.195 m
    #         * 1 μ° = 11.1195 cm
    radiusOfEarth = 6371008.8                                                   # [m]
    circumOfEarth = 2.0 * math.pi * radiusOfEarth                               # [m]

    lat_rad = math.radians(lat_deg)                                             # [rad]
    zoomOfEarth = math.log2(circumOfEarth * math.cos(lat_rad) / (256.0 * res))

    # Return answer ...
    return round(math.ceil(zoomOfEarth))
