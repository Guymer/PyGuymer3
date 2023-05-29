#!/usr/bin/env python3

# Define function ...
def find_middle_of_great_circle(lon1_deg, lat1_deg, lon2_deg, lat2_deg, /):
    """Calculate the middile of the great circle that connects two coordinates.

    This function reads in two coordinates (in degrees) on the surface of the
    Earth and calculates the middle of the great circle that connects them,
    correctly handling crossing over the anti-meridean (should it occur).

    Parameters
    ----------
    lon1 : float
        the longitude of the first coordinate (in degrees)
    lat1 : float
        the latitude of the first coordinate (in degrees)
    lon2 : float
        the longitude of the second coordinate (in degrees)
    lat2 : float
        the latitude of the second coordinate (in degrees)

    Returns
    -------
    lon3 : float
        the longitude of the middle of the great circle (in degrees)
    lat3 : float
        the latitude of the middle of the great circle (in degrees)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import math

    # Check arguments ...
    if lon1_deg == lon2_deg and lat1_deg == lat2_deg:
        return lon1_deg, lat1_deg

    # Convert to radians ...
    lon1_rad = math.radians(lon1_deg)                                           # [rad]
    lat1_rad = math.radians(lat1_deg)                                           # [rad]
    lon2_rad = math.radians(lon2_deg)                                           # [rad]
    lat2_rad = math.radians(lat2_deg)                                           # [rad]

    # Calculate mid-point ...
    Bx = math.cos(lat2_rad) * math.cos(lon2_rad - lon1_rad)
    By = math.cos(lat2_rad) * math.sin(lon2_rad - lon1_rad)
    lat3_rad = math.atan2(
        math.sin(lat1_rad) + math.sin(lat2_rad),
        math.sqrt((math.cos(lat1_rad) + Bx) * (math.cos(lat1_rad) + Bx) + By * By)
    )                                                                           # [rad]
    lon3_rad = lon1_rad + math.atan2(By, math.cos(lat1_rad) + Bx)               # [rad]

    # Return mid-point ...
    return math.degrees(lon3_rad), math.degrees(lat3_rad)
