#!/usr/bin/env python3

# Define function ...
def res(lat_deg, zoom):
    # NOTE: https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale

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

    lat_rad = math.radians(lat_deg)
    n = 2.0 ** zoom
    resoluOfEarth = circumOfEarth * math.cos(lat_rad) / (256.0 * n)             # [m/px]

    # Return answer ...
    return resoluOfEarth
