#!/usr/bin/env python3

# Import standard modules ...
import math

# Import special modules ...
try:
    import shapely
    import shapely.geometry
except:
    raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

# Set constants ...
EARTH = shapely.geometry.polygon.Polygon(
    shapely.geometry.polygon.LinearRing(
        [
            (-180.0, +90.0),
            (-180.0, -90.0),
            (+180.0, -90.0),
            (+180.0, +90.0),
            (-180.0, +90.0),
        ]
    )
)
EARTH_MOON_DISTANCE = 385000000.0                                               # [m]
RADIUS_OF_EARTH = 6371008.8                                                     # [m]

# Set derived constants ...
CIRCUMFERENCE_OF_EARTH = 2.0 * math.pi * RADIUS_OF_EARTH                        # [m]
DIAMETER_OF_EARTH = 2.0 * RADIUS_OF_EARTH                                       # [m]
RESOLUTION_OF_EARTH = CIRCUMFERENCE_OF_EARTH / 360.0                            # [m/°]
SURFACE_AREA_OF_EARTH = math.pi * pow(RADIUS_OF_EARTH, 2)                       # [m]

# Set limits ...
# NOTE: Running "python3.12 scripts/plotLimitOfVincenty.py --degree-interval 1"
#       tells me that the minimum maximum distance which the Vincenty formula
#       works is 19,970 km (which, looking at the associated PNG, occurs for
#       origins on the equator).
MAXIMUM_VINCENTY = 19970.0e3                                                    # [m]
