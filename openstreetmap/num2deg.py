def num2deg(xtile, ytile, zoom):
    # NOTE: http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Python
    import math
    n = 2.0 ** zoom
    lon_deg = xtile / n * 360.0 - 180.0
    lat_rad = math.atan(math.sinh(math.pi * (1.0 - 2.0 * ytile / n)))
    lat_deg = math.degrees(lat_rad)
    return lat_deg, lon_deg
