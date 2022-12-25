def hfov(xtile, ytile, zoom):
    # NOTE: https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale

    # Import standard modules ...
    import math

    # Import sub-functions ...
    from .num2deg import num2deg

    # Define contants  ...
    # NOTE: See https://en.wikipedia.org/wiki/Earth_radius#Mean_radius
    # NOTE: Using a radius of 6,371,008.8 m equates to:
    #         * 1 °  = 111.195 km
    #         * 1 m° = 111.195 m
    #         * 1 μ° = 11.1195 cm
    radiusOfEarth = 6371008.8                                                   # [m]
    circumOfEarth = 2.0 * math.pi * radiusOfEarth                               # [m]

    lonW_deg, latN_deg = num2deg(xtile, ytile, zoom)                            # [°], [°]
    lonE_deg, latS_deg = num2deg(xtile + 1, ytile + 1, zoom)                    # [°], [°]
    latC_rad = math.radians(0.5 * (latS_deg + latN_deg))
    n = 2.0 ** zoom
    resoluOfEarth = circumOfEarth * math.cos(latC_rad) / n                      # [m/tile]

    # Return answer ...
    return lonE_deg - lonW_deg, resoluOfEarth
