#!/usr/bin/env python3

# Import standard modules ...
import math

# Set constants ...
EARTH_MOON_DISTANCE = 385000000.0                                               # [m]
RADIUS_OF_EARTH = 6371008.8                                                     # [m]

# Set derived constants ...
CIRCUMFERENCE_OF_EARTH = 2.0 * math.pi * RADIUS_OF_EARTH                        # [m]
RESOLUTION_OF_EARTH = CIRCUMFERENCE_OF_EARTH / 360.0                            # [m/°]

# Set limits ...
# NOTE: Running "python3.12 scripts/plotLimitOfVincenty.py --degree-interval 1"
#       tells me that the minimum maximum distance which the Vincenty formula
#       works is 19,970 km (which, looking at the associated PNG, occurs for
#       origins on the equator).
MAXIMUM_VINCENTY = 19970.0e3                                                    # [m]
