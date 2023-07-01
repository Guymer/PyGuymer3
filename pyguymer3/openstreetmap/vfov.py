#!/usr/bin/env python3

# Define function ...
def vfov(xtile, ytile, zoom, /):
    # NOTE: https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale

    # Import standard modules ...
    import math

    # Import sub-functions ...
    from .num2deg import num2deg
    from ..consts import CIRCUMFERENCE_OF_EARTH

    _, latN_deg = num2deg(xtile, ytile, zoom)                                   # [°], [°]
    _, latS_deg = num2deg(xtile + 1, ytile + 1, zoom)                           # [°], [°]
    latC_rad = math.radians(0.5 * (latS_deg + latN_deg))
    n = pow(2, zoom)
    resoluOfEarth = CIRCUMFERENCE_OF_EARTH * math.cos(latC_rad) / n             # [m/tile]

    # Return answer ...
    return latN_deg - latS_deg, resoluOfEarth
