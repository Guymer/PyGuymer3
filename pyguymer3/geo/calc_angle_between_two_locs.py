#!/usr/bin/env python3

# Define function ...
def calc_angle_between_two_locs(lon1_deg, lat1_deg, lon2_deg, lat2_deg, /):
    """Calculate the angle between two coordinates.

    This function reads in two coordinates (in degrees) on the surface of a
    sphere and calculates the angle (in degrees) between them, as viewed from
    the centre of the sphere.

    Parameters
    ----------
    lon1_deg : float
        the longitude of the first coordinate (in degrees)
    lat1_deg : float
        the latitude of the first coordinate (in degrees)
    lon2_deg : float
        the longitude of the second coordinate (in degrees)
    lat2_deg : float
        the latitude of the second coordinate (in degrees)

    Returns
    -------
    angle : float
        the angle between the two coordinates (in degrees)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import math

    # Convert to radians ...
    lon1_rad = math.radians(lon1_deg)                                           # [rad]
    lat1_rad = math.radians(lat1_deg)                                           # [rad]
    lon2_rad = math.radians(lon2_deg)                                           # [rad]
    lat2_rad = math.radians(lat2_deg)                                           # [rad]

    # Calculate angle in radians ...
    distance_rad = 2.0 * math.asin(
        math.hypot(
            math.sin((lat1_rad - lat2_rad) / 2.0),
            math.cos(lat1_rad) * math.cos(lat2_rad) * math.sin((lon1_rad - lon2_rad) / 2.0)
        )
    )                                                                           # [rad]

    # Return angle ...
    return math.degrees(distance_rad)
