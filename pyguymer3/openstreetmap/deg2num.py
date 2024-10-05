#!/usr/bin/env python3

# Define function ...
def deg2num(
    lon_deg,
    lat_deg,
    zoom,
    /,
):
    # NOTE: https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Python

    # Import standard modules ...
    import math

    lat_rad = math.radians(lat_deg)
    n = pow(2, zoom)
    xtile = int((lon_deg + 180.0) / 360.0 * n)
    ytile = int((1.0 - math.asinh(math.tan(lat_rad)) / math.pi) / 2.0 * n)

    # Return answer ...
    return xtile, ytile
